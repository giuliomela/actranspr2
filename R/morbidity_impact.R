#' Compute the overall health impact of active mobility on both mortality and morbidity
#'
#' This function calculates the impact of a shift from passive to active mobility on morbidity and
#' mortality for Italian municipalities taking into account the effect of increased physical activity,
#' increased air pollution intake and increased road injury risk.
#'
#' @inheritParams scenario
#' @param working_weeks Number of annual working weeks. Default is `48`
#' @param detail If set to `low`, the default, the function returns results the overall health impact expressed
#'     in euro/km. Other options are: `medium` and `high`, which return results with detail at increasing level
#'     of detail: by dimension (physical activity, air pollution and road injuries) or by disease/outcome respectively.
#' @param sdr The social discount rate to be used to discount future YLL and YLD. The default is `0.024`.
#'     The function `sdr` of the `btransfer` package can be used to compute the social discount rate
#' @param ref_year Year of reference of the analysis. Default is `2021`
#' @param voly,vsl The value of the VOLY or the VSL to be used. Values adjusted to a user-defined policy year can
#'     be computed with function `actranspr2::voly_vsl_calculator`
#' @return A tibble
#' @importFrom rlang .data
#' @export
#' @examples
#' morbidity_impact(
#' mode_from = "car",
#' mode_to = "ebike",
#' detail = "medium"
#' )
morbidity_impact <- function(city = "Roma",
                             min_age = 20, max_age = 64,
                             mode_from = "car", mode_to = "bike",
                             share.less_3km = 0.3, share.3_10km = 0.2, share.10_15km = 0,
                             share.more_15km = 0,
                             commuting_days = 4,
                             experimental_data = FALSE,
                             met_phy_act = 7,
                             working_weeks = 48,
                             detail = "low",
                             sdr = 0.024,
                             ref_year = 2021,
                             voly = 70151,
                             vsl = 3607757
){

  if(!is.element(detail, c("high", "medium", "low")))
    stop("The 'detail' argument can assume three values only: high, medium or low")

  scenario <- actranspr2::scenario( #creating a mobility-change scenario
    city = {{city}},
    mode_from = {{mode_from}},
    mode_to = {{mode_to}},
    share.less_3km = {{share.less_3km}},
    share.3_10km = {{share.3_10km}},
    share.10_15km = {{share.10_15km}},
    share.more_15km = {{share.more_15km}},
    commuting_days = {{commuting_days}},
    met_phy_act = {{met_phy_act}},
    experimental_data = {{experimental_data}}
  )

  # computing total km travelled each year

  scenario$km_year <- scenario$km_round_trip * scenario$new_adopters * working_weeks * commuting_days

  tot_km_year  <- sum(scenario$km_year)

  # Computing physical activity RR

  rr_phy <- dose_res_phy(
    baseline_met_week = scenario$phy_act_weekly_met,
    scenario_met_week = scenario$total_weekly_met,
    mode_from = mode_from,
    mode_to = mode_to,
    dimension = "morbidity"
  )

  # computing air pollution rr

  rr_air <- dose_res_air(
    city = {{city}},
    mode_from = {{mode_from}},
    mode_to = {{mode_to}},
    w_phy_act = scenario$phy_act_weekly_h,
    w_commuting_from = scenario[[paste0(mode_from, "_weekly_travel_time")]],
    w_commuting_to = scenario[[paste0(mode_to, "_weekly_travel_time")]],
    experimental_data = {{experimental_data}},
    working_weeks = {{working_weeks}},
    dimension = "morbidity"
  )

  incidence <- actranspr2::disease_incidence[actranspr2::disease_incidence$sex_name == "Both",
                                          c("cause_name", "val", "ages")]

  names(incidence) <- c("outcome", "incidence", "age")


  phy_diseases <- length(unique(rr_phy$outcome))

  air_diseases <- length(unique(rr_air$outcome))

  impact_phy <- do.call("rbind", replicate(phy_diseases, scenario, simplify = FALSE)) |>
    dplyr::bind_cols(rr_phy) |>
    dplyr::mutate(dimension = "phy")

  impact_air <- do.call("rbind", replicate(air_diseases, scenario, simplify = FALSE)) |>
    dplyr::bind_cols(rr_air) |>
    dplyr::mutate(dimension = "air")

  impact_l <- list(impact_air, impact_phy)

  impact <- dplyr::bind_rows(impact_l)

  impact <- impact |>
    dplyr::left_join(incidence) |>
    dplyr::mutate(avoided_cases = .data$new_adopters * .data$incidence * !!as.name(paste0("rr_", mode_from, "_to_", mode_to)),
                  avoided_cases = ifelse(
                    .data$dimension == "phy",
                    .data$avoided_cases,
                    .data$avoided_cases * -1
                  ),
                  incidence = NULL) |>
    dplyr::select(!paste0("rr_", {{mode_from}}, "_to_", {{mode_to}}))

  impact


  # Computing injury rates

  inj_mode <- ifelse( # this is because statistics on road injuries involving ebikes are not reliable just yet
    mode_to == "ebike",
    "bike",
    mode_to
  )

  inj_rate <- actranspr2::road_inj_rate |>
    dplyr::filter(city == {{city}} & mode %in% c({{mode_from}}, inj_mode)) |>
    dplyr::select("age_class", "outcome", "mode", "road_inj_rate") |>
    tidyr::pivot_wider(names_from = "mode", values_from = "road_inj_rate") |>
    dplyr::group_by(.data$age_class) |>
    tidyr::nest()


  inj_impact <- scenario |>
    dplyr::left_join(actranspr2::age_classes) |>
    dplyr::left_join(inj_rate) |>
    tidyr::unnest(.data$data)

  inj_impact$diff_road_inj_rate <- inj_impact[[mode_from]] - inj_impact[[inj_mode]]

  inj_impact

  inj_impact[[mode_from]] <- NULL

  inj_impact[[inj_mode]] <- NULL

  inj_impact <- inj_impact |>
    dplyr::mutate(avoided_cases = .data$km_year * .data$diff_road_inj_rate,
                  dimension = "inj") |>
    dplyr::select(!c("age_class", "diff_road_inj_rate"))

impact <- dplyr::bind_rows(inj_impact, impact)


  # adding life expectancy data

  # nuts2_code <- actranspr2::codici_comuni_2023 |>
  #   dplyr::filter(.data$`Denominazione in italiano` == {{city}}) |>
  #   dplyr::pull(.data$`Codice NUTS2 2021 (3)`)

  life_exp <- dplyr::select(actranspr2::demo_data,
                            "city", "age", "life_exp") |>
    dplyr::filter(.data$city == {{city}})
  # |>
  #   dplyr::group_by(.data$age) |>
  #   dplyr::summarise(life_exp = mean(.data$life_exp)) |>
  #   dplyr::ungroup()
  #   # dplyr::ungroup() |>
    # unique()

impact <- impact |>
  dplyr::left_join(life_exp)


# adding disease length data

dis_length <- dplyr::select(actranspr2::disease_length,
                                "disease", "cause_life_exp_decrease" ,
                                "duration_years", "lifelong",
                                "life_exp_decrease", "ages") |>
  dplyr::mutate(outcome = .data$disease,
                age = .data$ages,
                .keep = "unused")

impact <- impact |>
  dplyr::left_join(dis_length) |>
  dplyr::mutate(disease_length = dplyr::case_when(
    .data$lifelong == "yes" ~ .data$life_exp + .data$life_exp_decrease,
    .data$lifelong == "no" ~ .data$duration_years
  ))

# adding disability weights

dis_weights <- NULL

for (i in c("acute", "chronic")) {

  dis_weights[[i]] <- actranspr2::disability_weights |>
    dplyr::filter(.data$phase == i) |>
    dplyr::select("disease", "disability_weight")

  names(dis_weights[[i]]) <- c("outcome", i)

}

#adding disability weights

impact <- impact |>
  dplyr::left_join(dis_weights[["acute"]]) |>
  dplyr::left_join(dis_weights[["chronic"]])

#Life years gained/lost

impact$delta_life_yr <- purrr::map2_dbl(
  impact$life_exp_decrease * -1,
  impact$disease_length,
  function(x, y) compute_yll(
    disease_length = y,
    life_exp_red = x,
    sdr = sdr
  )
) * impact$avoided_cases

# Additional/avoided disability years

impact$delta_dis_yr <- purrr::pmap_dbl(
  list(
    impact$acute,
    impact$chronic,
    impact$disease_length
  ), function(x, y, z) compute_yld(
    acute_dw = x,
    chronic_dw = y,
    disease_length = z,
    sdr = sdr
  )
) * impact$avoided_cases


# monetisation

impact <- impact |>
  dplyr::group_by(.data$city, .data$outcome, .data$dimension) |>
  dplyr::summarise(dplyr::across(c(.data$delta_life_yr, .data$delta_dis_yr),
                                 function(x) sum(x * {{voly}}))) |>
  dplyr::ungroup() |>
  dplyr::mutate(dplyr::across(c(.data$delta_life_yr, .data$delta_dis_yr),
                              function(x) x / tot_km_year,
                              .names = "{.col}_km"),
                mode_from = {{mode_from}},
                mode_to = {{mode_to}}) |>
  dplyr::select("city", "mode_from", "mode_to", dplyr::everything())


# traumatic deaths impact

fatal_road_inj_rate <- actranspr2::fatal_road_inj_rate |>
  dplyr::filter(city == {{city}} & mode %in% c({{mode_from}}, inj_mode)) |>
  dplyr::select("age_class", "mode", "fatal_road_inj_rate") |>
  tidyr::pivot_wider(names_from = .data$mode, values_from = .data$fatal_road_inj_rate)

fatal_inj_impact <- scenario |>
  dplyr::left_join(age_classes) |>
  dplyr::left_join(fatal_road_inj_rate)

fatal_inj_impact$diff_fatal_inj_rate <- fatal_inj_impact[[mode_from]] - fatal_inj_impact[[inj_mode]]

fatal_inj_impact[[mode_from]] <- NULL

fatal_inj_impact[[inj_mode]] <- NULL

fatal_inj_impact <- fatal_inj_impact |>
  dplyr::mutate(avoided_deaths =  .data$km_year * .data$diff_fatal_inj_rate * {{vsl}}, # monetisation included
                dimension = "inj") |>
  dplyr::select(!c("age_class", "diff_fatal_inj_rate"))

delta_traumatic_deaths_km <- sum(fatal_inj_impact$avoided_deaths) / tot_km_year

if (detail == "high"){

  impact |>
   # dplyr::select(!"delta_life_yr", "delta_dis_yr") |>
    dplyr::add_row(
      city = {{city}},
      mode_from = {{mode_from}},
      mode_to = {{mode_to}},
      outcome = "traumatic death",
      dimension = "inj",
      delta_life_yr_km = NA_real_,
      delta_dis_yr_km = NA_real_
    ) |>
    dplyr::mutate(delta_traumatic_deaths_km = ifelse(
      .data$dimension == "inj" & .data$outcome == "traumatic death",
      delta_traumatic_deaths_km,
      NA_real_
    )) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ ifelse(is.na(.x),
                                                                   0,
                                                                   .x)))

} else if (detail == "medium"){

  impact |>
    dplyr::group_by(.data$dimension) |>
    dplyr::summarise(dplyr::across(dplyr::contains("km"), ~ sum(., na.rm = T))) |>
    dplyr::ungroup() |>
    dplyr::mutate(tram_deaths_km = ifelse(
      .data$dimension == "inj",
      delta_traumatic_deaths_km,
      0
    ))


} else if (detail == "low") {

  impact <- impact |>
    dplyr::group_by(.data$dimension) |>
    dplyr::summarise(dplyr::across(dplyr::contains("km"), ~ sum(., na.rm = T))) |>
    dplyr::ungroup() |>
    dplyr::mutate(tram_deaths_km = ifelse(
      .data$dimension == "inj",
      delta_traumatic_deaths_km,
      0
    ))

  impact |>
    dplyr::select(dplyr::contains("km")) |>
    sum(na.rm = TRUE)

}


}
