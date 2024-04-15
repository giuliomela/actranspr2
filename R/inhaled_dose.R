#' Compute the inhaled dose of air pollutants by individuals
#'
#' This function computes the daily inhaled dose of PM2.5 for individuals living in
#' different Italian municipalities. The function takes into account the commuting mode and
#' the weekly hours of commuting and physical activity. Total inhaled doses are calculated following Engström and Forsberg (2019). It can return the ration between the inhaled dose
#' in the commuting scenario (i.e. by car or bike) and a baseline scenario (no commuting at all).
#'
#' @param city A single character string. The name (in Italian) of the Italian muncipality for which the
#'     scenario must be built. THe full list of city names can be found in the commuting matrix
#'     (see `comm_matrix_cities_km`)
#' @param mode A character string. The transportation mode of interest:  it can assume the following values:
#'     `walk`, `bike` and `ebike`.
#' @param w_phy_act The weekly hours of physical activity.
#' @param w_commuting The weekly hours of commuting.
#' @param commuting_days Number of weekly commuting days. Default is `4`
#' @param experimental_data A logical value. If set to `TRUE` data from the RSE experimentation are used.
#'     Default set to `FALSE`
#' @param working_weeks Number of annual working weeks
#' @param eq_change whether the function returns the equivalent change or not
#' @return Either a vector or a number depending on the `ratio` parameter
inhaled_dose <- function(city = "Roma",
                         mode = "car",
                         w_phy_act = 5,
                         w_commuting = 4,
                         experimental_data = FALSE,
                         working_weeks = 48,
                         commuting_days = 4,
                         eq_change = TRUE) {

  hours <- conf_fct <- inhaled <- bpm <- kcal <- h <- kg <- activity <- trans_mode <- NULL

  # finding background concentrations

  bkg_conc <- pm25_concentration(place = city)

  #Defining which ventilation data to use

  if(isTRUE(experimental_data)){

    if(!is.element(mode, c("bike", "ebike", "walk")))
      stop("If experimental_data is set to TRUE, mode must be selected among bike, ebike and walk only.")

    exp_vr <- exp_data %>%
      dplyr::filter(mode == {{mode}})

      exp_vr <- exp_vr %>%
        dplyr::mutate(vr = purrr::map_dbl(bpm, function(x) met_vr_calculator(
          var = "vr", bpm = x
        ))) %>%
        dplyr::pull() %>%
        mean(na.rm = TRUE)

      vr <- vent_rates %>%
        dplyr::mutate(vent_rates = ifelse(activity == mode,
                                          exp_vr,
                                          vent_rates))

  } else if (isFALSE(experimental_data)){

    vr <- vent_rates

  }

  vr <- vr %>%
    dplyr::filter(activity %in% c(mode, "rest", "phy", "sleep"))

  vr_fct <- vr$vent_rates * vr$conf_fct

  names(vr_fct) <- vr$activity

  times <- purrr::map(c(w_commuting, 0),
                      function(x) {
                        comm <- x / 7
                        sleep <- 8
                        phy <- w_phy_act / 7
                        rest <- 24 - sleep - phy - comm
                        out <- c(comm, rest, sleep, phy)
                        names(out) <- c(mode, "rest", "sleep", "phy")
                        out
                      }
  ) %>%
    stats::setNames(c(mode, "baseline"))


  inhaled <- purrr::map(times,
                        function(x)
                          sum(x * bkg_conc * vr_fct)
  )


  #Computing annual doses

  inhaled_annual <- NULL

  for (i in c(mode, "baseline")){

    inhaled_annual[[i]] <- ifelse(
      i == mode,
      inhaled[[i]] * working_weeks * commuting_days + inhaled[["baseline"]] * (365.25 - working_weeks * commuting_days),
      inhaled[["baseline"]] * 365.25
    )

  }

  if(isTRUE(eq_change)){

    ((inhaled_annual[[mode]] / inhaled_annual[["baseline"]]) - 1) * bkg_conc

  } else {

    inhaled_annual

  }


}
