#' Create a mode-changing commuting scenario for Italian municipalities
#'
#' This function creates a scenario of transportation mode change for Italian municipalities based on the
#' 2011 Italian commuting matrix. The function returns a tibble with the number of active mobility adopters
#' according to the parameters chosen by the user (see below).
#'
#' @inheritParams mode_change_share
#' @param min_age,max_age Numeric values. The minimum and the maximum ages commuters can have to be included in the scenario.
#' @param city A single character string. The name (in Italian) of the Italian muncipality for which the
#'     scenario must be built. THe full list of city names can be found in the commuting matrix
#'     (see `data_raw$comm_matrix_cities_km`)
#' @param mode_from,mode_to Character strings. The transportation mode from which commuters shift from. And
#'     the transportation mode to which they shift to. Argument `mode_to` can assume the following values:
#'     `walk`, `bike` and `ebike`. Argument `mode_from` can assume any value amongst those included in
#'     the commuting matrix: `r knitr::combine_words(setdiff(data_raw$transport_speeds$mean_of_transp, c("walk", "bike", "ebike")))`
#' @param commuting_days A numeric value. The number of days in which new adopters perform travels to and
#'     from the place of work/study.
#' @param met_phy_act A numeric value. The physical effort (measured in MET/h) associated with physical
#'     activity performed by individuals in their free time (default is set to `7`).
#' @param experimental_data A logical value. If set to `FALSE`, the default, MET values are taken
#'     from the literature. If set to `TRUE` values from the RSE experimentation are used.
#' @return A tibble with the chosen parameters.
#' @export
#' @examples
#' scenario("Padova", "car", "ebike")
scenario <- function(city = "Roma",
                     mode_from = "car", mode_to = "bike",
                     share.less_3km = 0.3, share.3_10km = 0.2, share.10_15km = 0,
                     share.more_15km = 0, commuting_days = 4, experimental_data = FALSE,
                     met_phy_act = 7,
                     min_age = 20, max_age = 64
                     ) {

    # km_one_way <- km_round_trip <- speed_kmh <- share <- individuals <- nuts2 <- nuts3 <-
    # avg_travel_time <- mode_to_weekly_travel_time <- new_adopters <- age_share <- age <- pop_evo <- year <-
    # age_evo <- commuting_weekly_met <- share_phy_act <- actual_commuting_weekly_met <- phy_act_weekly_h <-
    # kcal <- h <- kg <- data <- phy_act_weekly_met <- NULL

  comm_matrix <- actranspr2::comm_matrix_cities_km %>%
    dplyr::filter(city == {{city}} & mode == {{mode_from}})

  scenario <- comm_matrix %>%
    dplyr::mutate(share = sapply(.data$km_one_way, function(x) mode_change_share(km_one_way = x,
                                                                           share.less_3km = {{share.less_3km}},
                                                                           share.3_10km = {{share.3_10km}},
                                                                           share.10_15km = {{share.10_15km}},
                                                                           share.more_15km = {{share.more_15km}})),
                  mode_to = {{mode_to}})





  scenario[[paste0(mode_to, "_weekly_travel_time")]] <- scenario$km_round_trip / scenario$speed_kmh *
    commuting_days

  scenario[[paste0(mode_from, "_weekly_travel_time")]] <- scenario$avg_travel_time / 60 * commuting_days


  scenario <- scenario %>%
    dplyr::filter(.data$share != 0) %>%
    dplyr::mutate(
      new_adopters = .data$individuals * .data$share,
      individuals = NULL
    )



  mob_change_scenario <- dplyr::select(scenario,
                                       "city", "nuts2", "nuts3", "avg_travel_time",
                                       "km_round_trip", "mode", "mode_to", "share", paste0(mode_from, "_weekly_travel_time"),
                                       paste0(mode_to, "_weekly_travel_time"), "new_adopters") %>%
    dplyr::mutate(share_new_mode = .data$share,
                  .keep = "unused")

  # adding MET data

  if (experimental_data == FALSE) {

    met <- actranspr2::met_lit

  } else if (experimental_data == TRUE) {

    if(nrow(dplyr::filter(exp_data, .data$mode == {{mode_to}})) == 0){

      met <- met_lit

    } else {

      exp_met <- actranspr2::exp_data %>%
        dplyr::filter(.data$mode == {{mode_to}}) %>%
        dplyr::mutate(met = purrr::pmap_dbl(list(.data$kcal, .data$kg, .data$h),
                                            function(x, y, z) met_vr_calculator(
                                              kcal = x,
                                              weigth = y,
                                              time = z
                                            ))) %>%
        dplyr::pull(.data$met) %>%
        mean(na.rm = T)


      met <- met_lit %>%
        dplyr::mutate(met = ifelse(
          .data$mode == {{mode_to}},
          .data$exp_met,
          .data$met
        ))

    }

  }

  mob_change_scenario <- mob_change_scenario %>%
    dplyr::left_join(met, by = c("mode_to" = "mode"))

  mob_change_scenario$commuting_weekly_met <- mob_change_scenario$met *
    mob_change_scenario[[paste0(mode_to, "_weekly_travel_time")]] # computing additional METs due to active mobility

  # splitting new adopters according to the demographic structure of the municipality of interest

  pop_str <- actranspr2::pop_latest %>%
    dplyr::filter(city == {{city}},
                  .data$age %in% c({{min_age}}:{{max_age}})) %>%
    dplyr::group_by(.data$city) %>%
    tidyr::nest()

  mob_change_scenario <- mob_change_scenario %>%
    dplyr::left_join(pop_str) %>%
    tidyr::unnest("data") %>%
    dplyr::mutate(new_adopters = .data$new_adopters * .data$pop_share,
                  .keep = "unused")

     physical_activity <- actranspr2::phy_act %>%
      dplyr::group_by(.data$age) %>%
      tidyr::nest()


    mob_change_scenario <- mob_change_scenario %>%
      dplyr::left_join(physical_activity) %>%
      tidyr::unnest("data") %>%
      dplyr::mutate(new_adopters = .data$new_adopters * .data$share_phy_act / 100,
                    phy_act_weekly_met = .data$phy_act_weekly_h * {{met_phy_act}},
                    total_weekly_met = .data$phy_act_weekly_met + .data$commuting_weekly_met) %>%
      dplyr::select("city", "nuts2", "nuts3", "km_round_trip", paste0(mode_to, "_weekly_travel_time"),
                    paste0(mode_from, "_weekly_travel_time"),
                    "new_adopters", "age", "total_weekly_met", "phy_act_weekly_met", "phy_act_weekly_h")


  mob_change_scenario

}
