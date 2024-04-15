# code to prepare `DATASET` dataset goes here

library(tidyverse)
library(here)
library(xlsx)
library(readr)
library(readxl)
library(rdbnomics)

pandemic_yr <- 2020 # pandemic year (to be escluded from some computations)

# Loading RAW data from excel (data_raw.xlsx)

data_raw_path <- here("data-raw", "data_raw.xlsx")

data_raw_sheets <- excel_sheets(data_raw_path)

data_raw <- lapply(data_raw_sheets,
                   function(x) read_excel(data_raw_path,
                                          sheet = x))

names(data_raw) <- data_raw_sheets # a names list with all raw data imported from Excel

list2env(data_raw, envir=.GlobalEnv)

# list of province capitals

prov_capitals <- unique(regional_capitals$prov_capital)

nuts_codes <- codici_comuni_2023 %>%
  select(city = `Denominazione in italiano`,
         nuts2 = `Codice NUTS2 2021 (3)`,
         nuts3 = `Codice NUTS3 2021`, province = Provincia)

prov_capitals <- regional_capitals %>%
  left_join(nuts_codes) %>%
  filter(city %in% prov_capitals)

#Loading the commuting matrix and preparing it for use

# loading ISTAT's commuting matrix

# commuting_matrix <- read_table(here("data-raw/commuting_matrix.txt"), col_names = FALSE) %>%
#   select(1:15)

commuting_matrix <- readRDS(here::here("data-raw", "commuting_matrix.rds"))

# Renaming the commuting matrix's variables

names(commuting_matrix) <- c("record_type", "residence_type", "prov_code", "mun_code",
                             "sex", "travel_reason", "place_of_destination", "province_work", "municipality_work", "country_work",
                             "mean_of_transp", "leaving_hour", "travel_time", "individuals_estimate",
                             "individuals")

# Municipality names, in the commuting matrix, are coded. For this reason it is
# necessary to pair them with municipality names.

# Loading municipality names (manually downloaded from ISTAT)

mun_codes <- codici_comuni_2011 %>%
  select(4, 7, 11, 12, 18:20) %>% # selecting only codes and names
  rename(code = 1, name = 2, region = 3, province = 4, nuts1 = 5,
         nuts2 = 6, nuts3 = 7) %>%
  mutate(name = if_else(name == "Roma Capitale", "Roma", name),
         nuts2 = if_else(province == "Trento",
                         "ITH2",
                         nuts2))

# Adding municipality names to the commuting matrix

commuting_matrix <- commuting_matrix %>%
  mutate(code = paste0(prov_code, mun_code),
         prov_code = NULL, mun_code = NULL) %>%
  left_join(mun_codes) %>%
  rename(city = name)

## Downloading life expectancy and mortality data at NUTS2 level

# life expectancy

nuts2_codes <- unique(codici_comuni_2023[["Codice NUTS2 2021 (3)"]])

ages <- paste0(
  "Y",
  10:84
)# ages to download

death_rt_life_exp <- expand_grid(
  nuts2_code = nuts2_codes,
  age = ages
)

death_rt_life_exp <- death_rt_life_exp %>%
  mutate(code_life_exp = paste0(
    "Eurostat/demo_r_mlifexp/A.YR.T.",
    age,
    ".",
    nuts2_code
  ),
  code_death_rate = paste0(
    "Eurostat/demo_r_mlife/A.DEATHRATE.T.",
    age,
    ".",
    nuts2_code
  )) #this tibble contains all codes to download data from the Eurostat database

# Downloading death rates and life expectancy data

demo_data <- NULL

for (i in c("life_exp", "death_rate")){

  demo_data[[i]] <- lapply(death_rt_life_exp[[paste0("code_", i)]],
                           function(x) rdb(x)) %>%
    bind_rows()

}

#Computing average death rate and life expectancy of the last 10 years

demo_data <- lapply(demo_data, function(x) x %>%
                      select(geo, year = original_period, age, value = original_value)) %>%
  bind_rows(.id = "var") %>%
  pivot_wider(names_from = var, values_from = value) %>%
  mutate(age = as.numeric(str_remove(age, "Y")),
         across(c(year, life_exp, death_rate), function(x) as.numeric(x))) %>%
  filter(year %in% c(max(year):max(year) - 9)) %>% # last 10 years only
  group_by(geo, age) %>%
  summarise(across(life_exp:death_rate, mean)) %>%
  ungroup()

demo_data <- demo_data %>%
  rename(nuts2 = geo) %>%
  group_by(nuts2) %>%
  nest() %>%
  left_join(prov_capitals) %>%
  select(city, data) %>%
  unnest(data)

# Manipulating the commuting matrix to extract relevant information
# although the original matrix distinguishes between male and female commuters, here they are summed up together

comm_matrix_light <- commuting_matrix %>%
  filter(record_type == "L") %>% # detailed data
  group_by(travel_reason, place_of_destination, code,
           city, region, province, nuts1, nuts2, nuts3,
           mean_of_transp, travel_time) %>% # performing some grouping, summing up males and females
  summarize(individuals = sum(as.numeric(individuals_estimate))) %>%
  ungroup() %>%
  filter(city %in% prov_capitals$city)

# Recoding the variables to make them human-readble

comm_matrix_light <- comm_matrix_light %>%
  mutate(
    travel_reason = if_else(travel_reason == 1, "study", "work"),
    place_of_destination = case_when(
      place_of_destination == 1 ~ "same municipality",
      place_of_destination == 2 ~ "other municipality",
      TRUE ~ "other country"
    ),
    mean_of_transp = case_when(
      mean_of_transp == "01" ~ "train",
      mean_of_transp == "02" ~ "tram",
      mean_of_transp == "03" ~ "subway",
      mean_of_transp == "04" ~ "urban_bus",
      mean_of_transp == "05" ~ "extra_urban_bus",
      mean_of_transp == "06" ~ "school_company_bus",
      mean_of_transp == "07" ~ "private_car_driver",
      mean_of_transp == "08" ~ "private_car_passenger",
      mean_of_transp == "09" ~ "motorbike_scooter",
      mean_of_transp == "10" ~ "bike",
      mean_of_transp == "11" ~ "other",
      TRUE ~ "walk"
    ),
    travel_time = case_when(
      travel_time == 1 ~ "<15",
      travel_time == 2 ~ "15_30",
      travel_time == 3 ~ "31_60",
      TRUE ~ ">60"
    )) %>%
  mutate(avg_travel_time = case_when( # Creating a numeric variable with the mean travel time by travel time category
    travel_time == "<15" ~ 7.5,
    travel_time == "15_30" ~ (15+30)/2,
    travel_time == "31_60" ~ (31+60)/2,
    TRUE ~ 60
  ))

#saveRDS(comm_matrix_light, here::here("comm_matrix_light.rds"))

# write_csv(comm_matrix_light, "comm_matrix_light.csv")

comm_matrix_light <- comm_matrix_light %>%
  mutate(mode = if_else(str_detect(mean_of_transp, "car"),
                        "car",
                        mean_of_transp)) %>%
  left_join(transport_speeds, by = c("mode" = "mean_of_transp")) %>%
  mutate(km_one_way = avg_travel_time / 60 * speed_kmh,
         km_round_trip = km_one_way * 2) %>% # eliminating the difference between car drivers and car passengers
  select(!c(mean_of_transp)) %>%
  group_by(code, city, region, province, nuts1, nuts2, nuts3, travel_time,
           avg_travel_time, speed_kmh, km_one_way, km_round_trip, mode, travel_reason, place_of_destination) %>%
  summarise(individuals = sum(individuals)) %>%
  ungroup()

comm_matrix_cities_km <- comm_matrix_light %>%
  filter(travel_reason == "work" &
           place_of_destination == "same municipality")

#### Road injury database

# column number of the variable to retain
var_to_retain <- c(1, 2, 3, 16, 17, 18, 28, 49, 70, 89, 91, 93, 95, 97, 99, 101, 103, 6)

age_classes_inj <- c(26, 32, 35, 38, 41, 47, 53, 56, 59, 62, 68, 74, 77, 80, 83, 90, 92, 94,
                     96, 98, 100, 102, 104)

var_to_retain <- sort(union(var_to_retain, age_classes_inj))

years <- c(2018:2021)

# injury_data_raw <- purrr::map_dfr(years,
#                                   function(x) {read.table(here::here("data-raw", "road_accidents",
#                                                                      paste0("INCSTRAD_Microdati_", x, ".txt")),
#                                                           header = T,
#                                                           sep = "\t") %>%
#                                       dplyr::select(dplyr::all_of(var_to_retain))
#                                   }
# )
#
### saveRDS(injury_data_raw, here::here("data-raw", "injury_data_raw.rds"))

injury_data_raw <- readRDS(here::here("data-raw", "injury_data_raw.rds"))

injury_data_raw <- injury_data_raw |>
  filter(localizzazione_incidente %in% 0:3) |> # considering urban accidents only
  select(!localizzazione_incidente)
#%>% as_tibble()

vehicles_codes <- tibble(
  mode = 1:23,
  mode_label = c("car", "car", "car", "car", "urban_bus", "extra_urban_bus", "tram", "other", "other",
                 "other", "other", "other", "other", "bike", "motorbike_scooter", "motorbike_scooter",
                 "motorbike_scooter", "other", "other", "other", "other", "electric_scooter", "ebike")
)

all_modes <- injury_data_raw %>%
  filter(if_any(contains("esito"), ~ . %in% c(2, 3, 4))) %>%
  select(1:6, matches("et__conducente|esito_conducente"))

names_all_modes1 <- paste0(
  rep(letters[1:3], 3),
  "_",
  rep("mode", 3)
)

names_all_modes2 <- paste0(
  rep(letters[1:3], each = 2),
  "_",
  rep(c("age", "outcome"), 2)
)

names_all_modes <- union(names_all_modes1, names_all_modes2)

names(all_modes)[4:ncol(all_modes)] <- names_all_modes

all_modes_l <- NULL

for (i in letters[1:3]) {

  all_modes_l[[i]] <- all_modes[, c("anno", "provincia", "comune",
                                    paste0(i, "_mode"),
                                    paste0(i, "_age"), paste0(i, "_outcome"))]

  names(all_modes_l[[i]]) <- c("year", "prov", "city", "mode", "age", "outcome")

  all_modes_l[[i]] <- all_modes_l[[i]][!is.na(all_modes_l[[i]][["mode"]]), ]

}

all_modes_db <- all_modes_l %>%
  bind_rows(.id = "vehicle") %>%
  mutate(age = str_replace_all(age, " ", "")) %>%
  filter(!is.na(outcome)) %>% # filtering out records with outcome recorded as NA
  mutate(age = case_when(
    age == "n.i." ~ "45-54",
    age == "65+" ~ "65-120",  #replacing NA ages with the class of the italian median age
    TRUE~ age
  ))

all_modes_db <- all_modes_db %>%
  left_join(vehicles_codes) %>%
  mutate(mode = NULL) %>%
  rename(mode = mode_label)

all_modes_inj <- all_modes_db %>%
  filter(outcome != 1) %>%
  mutate(injury = if_else(
    outcome == 2,
    1,
    0
  ),
  death = if_else(outcome != 2,
                  1,
                  0),
  .keep = "unused") %>%
  group_by(year, prov, city, mode, age) %>%
  summarise(across(injury:death, sum)) %>%
  ungroup()

# pedestrian injuries

walk <- injury_data_raw %>%
  select(anno, provincia, comune, starts_with("pedone"))

walk_l <- NULL

for (i in 1:4) {

  walk_l[[i]] <- walk[, c("anno", "provincia", "comune",
                          paste0("pedone_ferito_", i, "__et_"),
                          paste0("pedone_morto_", i, "__et_"))]

  names(walk_l[[i]]) <- c("year", "prov", "city", "inj_age", "death_age")

}

walk <- walk_l %>%
  bind_rows(.id = "pedestrian") %>%
  mutate(across(inj_age:death_age, ~ str_remove_all(.x, " ")))

walk_l <- map(c("inj_age", "death_age"),
              ~ walk[, c("pedestrian", "year", "prov", "city", .x)])

walk_l <- map(walk_l, ~ rename(.x, age = 5))

names(walk_l) <- c("injury", "death")

walk_l <- map(walk_l, ~ .x[!is.element(.x[["age"]], c("", NA_character_)), ])

walk_inj <- walk_l %>%
  bind_rows(.id = "outcome") %>%
  mutate(age = case_when(
    age == "n.i." ~ "45-54",
    age == "65+" ~ "65-120",  #replacing NA ages with the class of the italian median age
    TRUE ~ age
  ),
  injury = if_else(outcome == "injury", 1, 0),
  death = if_else(outcome == "death", 1, 0),
  outcome = NULL
  ) %>%
  group_by(year, prov, city, age) %>%
  summarise(across(injury:death, sum)) %>%
  ungroup() %>%
  mutate(mode = "walk")

# preparing final injury tibble

inj_age_classes <- tibble(
  ETA1 = 0:99
) %>%
  mutate(age_class = case_when(
    ETA1 >= 0 & ETA1 <= 17 ~ "0-17",
    ETA1 >= 18 & ETA1 <= 29 ~ "18-29",
    ETA1 >= 30 & ETA1 <= 44 ~ "30-44",
    ETA1 >= 45 & ETA1 <= 54 ~ "45-54",
    ETA1 >= 55 & ETA1 <= 64 ~ "55-64",
    TRUE ~ "65-120"
  ),
  ETA1 = paste0("Y", ETA1)) %>%
  add_row(ETA1 = "Y_GE100", age_class = "65-120")

pop_classes <- pop_latest %>%
  filter(ETA1 != "TOTAL") %>%
  left_join(inj_age_classes) %>%
  group_by(Territory, age_class) %>%
  summarise(pop = sum(Value)) %>%
  ungroup() %>%
  rename(city = Territory, age = age_class)

# Preparing city and province codes

prov_mun_codes <- codici_comuni_2023 %>%
  select(prov = `Codice Provincia (Storico)(1)`,
         city = `Progressivo del Comune (2)`,
         city_name = `Denominazione in italiano`,
         prov_name = "Provincia")


prov_mun_codes <- expand_grid(prov_mun_codes,
                              mode = c("walk", unique(vehicles_codes$mode_label)),
                              age = unique(pop_classes$age),
                              year = 18:21)

all_inj <- walk_inj %>%
  bind_rows(all_modes_inj)

road_injuries <- prov_mun_codes %>%
  left_join(all_inj) %>%
  filter(age != "0-17",
         city_name %in% prov_capitals$city) %>%
  select(city = city_name, prov = prov_name, mode, age, year, injury, death) %>%
  left_join(pop_classes) %>%
  mutate(across(injury:death, ~ if_else(is.na(.x), 0, .x))) %>%
  group_by(city, prov, mode, age, pop) %>%
  summarise(across(injury:death, ~ mean(.x, na.rm = T))) %>%
  ungroup()

### Computing road injury exposure

#Adding mobility rate (age-dependent)

mobility_share <- mobility_shares_age %>%
  group_by(age) %>%
  summarise(mobility_share = mean(mob_share)) %>%
  ungroup()

mobility_share <- tibble(age_class = unique(road_injuries$age),
                         age = c("18-29", "30-45", "46-64", "46-64", "65-120")) %>%
  left_join(mobility_share) %>%
  select(age_class, mobility_share)

inj_exposure <- road_injuries %>%
  left_join(regional_capitals) %>%
  rename(age_class = age) %>%
  left_join(mobility_share)

# Adding mode share (% of travels with different modes of transportation)

# Computing mode shares from the commuting matrix (urban travels only)

mode_share <- comm_matrix_light %>%
  group_by(city, region, province, mode) %>%
  summarise(individuals = sum(individuals)) %>%
  mutate(share = individuals/sum(individuals)) %>%
  ungroup() %>%
  select(city = city, province, region, mode, mode_share = share)

inj_exposure <- inj_exposure %>%
  left_join(mode_share, multiple = "all")

# Adding mobility demand

mobility_demand <- mobility_shares %>%
  group_by(geo) %>%
  summarise(mobility_demand = mean(mobility_demand))

inj_exposure <- inj_exposure %>%
  left_join(mobility_demand)

# Adding trip length

avg_trip_length <- comm_matrix_light %>%
  group_by(city, province, region, mode) %>%
  summarise(avg_trip_length = weighted.mean(km_one_way, individuals)) %>%
  ungroup()

inj_exposure <- inj_exposure %>%
  left_join(avg_trip_length)

# Computing injury and death exposure

inj_exposure <- inj_exposure %>%
  mutate(exposure = pop * mobility_share * mode_share * mobility_demand * avg_trip_length * 365.25,
         .keep = "unused")

# Rate of fatal road injuries

fatal_road_inj_rate <- inj_exposure %>%
  mutate(fatal_road_inj_rate = death / exposure,
         .keep = "unused") %>%
  select(city, province, mode, age_class, region, geo, fatal_road_inj_rate)

# Road injury rates (by type of injury)

injury_incidence <- injury_incidence %>%
  mutate(cause = str_remove(cause, " road injuries")) %>%
  rename(outcome = disease, incidence = share) %>%
  group_by(cause) %>%
  nest()

road_inj_rate <- inj_exposure %>%
  mutate(cause = case_when(
    mode %in% c("urban_bus", "extra_urban_bus", "car") ~ "Motor vehicle",
    mode %in% c("tram", "other", "electric_scooter") ~ "Other",
    mode %in% c("bike", "ebike") ~ "Cyclist",
    mode == "motorbike_scooter" ~ "Motorcyclist",
    mode == "walk" ~ "Pedestrian"
  )) %>%
  left_join(injury_incidence) %>%
  unnest(data) %>%
  mutate(road_inj_rate = injury * incidence / exposure) %>%
  select(city, province, mode, age_class, region, geo, outcome, road_inj_rate)


# Downloading physical activity data from Eurostat
# Time spent on health-enhancing (non-work-related) aerobic physical activity by sex, age and educational attainment level [hlth_ehis_pe2e]

duration_codes <- c("MN0", "MN1-149", "MN150-299", "MN_GE300") # hours/week of physical activity

age_codes <- c("Y20-24", paste0(
  "Y",
  seq(25, 65, 10),
  "-",
  seq(34, 74, 10)
),
"Y_GE75"
) #age codes

phy_act_codes <- expand_grid(duration_codes, age_codes) %>%
  rowwise() %>%
  mutate(full_code = paste0(
    "Eurostat/hlth_ehis_pe2e/A.PC.",
    duration_codes,
    ".TOTAL.T.",
    age_codes,
    ".IT"
  )) %>%
  ungroup() %>%
  pull(full_code)

phy_act_raw <- purrr::map(phy_act_codes, function(x) rdb(x)) %>%
  bind_rows()

phy_act <- phy_act_raw %>%
  filter(original_period == "2019" & sex == "T") %>%
  select(age, duration, value) %>%
  mutate(age = if_else(age == "Y_GE75", "Y75-100", age), # transforming the higest class
         age = str_remove(age, "Y")) %>%
  separate(age, c("lower", "upper"), "-") %>%
  mutate(age = mapply(function(x, y) seq(x, y, 1), lower, upper),
         lower = NULL, upper = NULL) %>%
  unnest(age) %>%
  mutate(phy_act_weekly_h = case_when(
    duration == "MN0" ~ 0,
    duration == "MN1-149" ~ 150 / 2 / 60,
    duration == "MN150-299" ~ 300 / 2 / 60,
    duration == "MN_GE300" ~ 300 / 60
  )) %>%
  select(age, phy_act_weekly_h, share_phy_act = value)

# Manipulating road accident data

air_pollution_ita <-  air_pollution_ita %>%
  group_by(`City or Locality`) %>%
  summarise(across(`PM2.5 (μg/m3)`:`NO2 temporal coverage (%)`, ~ mean(.x, na.rm = T))) %>%
  ungroup() %>%
  select(city = `City or Locality`, pm25 = `PM2.5 (μg/m3)`) %>%
  filter(!is.na(pm25))

#Preparing Global Burden of Disease data

ages <- paste0(
  seq(15, 95, 5),
  "-",
  seq(19, 99, 5)
)

gbd_raw <- read.csv(here::here("data-raw", "gbd_data_2019.csv")) %>%
  as_tibble()

#computing disease incidence (cases every 100 inhabitants)

disease_incidence <- gbd_raw %>%
  mutate(age_name = str_remove(age_name, " years")) %>%
  filter(age_name %in% ages, measure_name == "Incidence",
         metric_name == "Rate") %>%
  separate(age_name, c("low_age", "upper_age"), "-") %>%
  rowwise() %>%
  mutate(ages = list(seq(low_age, upper_age, 1))) %>%
  ungroup() %>%
  select(sex_name, cause_name, val, upper, lower, ages) %>%
  unnest(ages) %>%
  mutate(across(c(val, lower, upper), ~ .x / 10^5)) # rates now expressed in relative share (es. 0.1 = 10%)

# fixing the disease length tibble

disease_length <- disease_length %>%
  separate(age, c("min", "max"), sep = "-", convert = T) %>%
  mutate(ages = map2(min, max, function(x, y) seq(x, y, 1))) %>%
  unnest(ages)

#Adding age classes key

age_classes <- tibble(
  age = 1:120
) %>%
  mutate(age_class = case_when(
    age < 18 ~ "1-17",
    age >= 18 & age < 30 ~ "18-29",
    age >= 30 & age < 45 ~ "30-44",
    age >= 45 & age < 55 ~ "45-54",
    age >= 55 & age < 64 ~ "55-64",
    TRUE ~ "65-120"
  ))

# Population data

pop_latest <- pop_latest %>%
  filter(ETA1 != "TOTAL", ETA1 != "Y_GE100",
         Territory %in% prov_capitals$city) %>%
  mutate(age = as.numeric(stringr::str_remove(ETA1, "Y"))) %>%
  select(city = Territory, age, pop = Value) %>%
  group_by(city) %>%
  mutate(pop_share = pop / sum(pop)) %>%
  ungroup()


usethis::use_data(demo_data, comm_matrix_cities_km, road_inj_rate, met_lit, exp_data,
                  air_pollution_ita, vent_rates, codici_comuni_2023, disability_weights,
                  pop_latest, phy_act, mortality_rr, morbidity_rr, regional_capitals,
                  fatal_road_inj_rate, disease_incidence, disease_length, age_classes, road_injuries,
                  overwrite = TRUE, internal = FALSE)
