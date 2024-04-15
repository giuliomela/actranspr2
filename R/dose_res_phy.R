#' Dose-response function - physical activity
#'
#' This function computes dose-response functions for physical activity . Such function
#' relates exposure to physical activity to health outcomes (namely incidence of some
#' non-communicable diseases). The output is a tibble in which the `rr_modefrom_to_modeto` column is the
#' coefficient to be used to calculate avoided cases from "general population" incidence.
#'
#' @param city A single character string. The name (in Italian) of the Italian municipality for which the
#'     scenario must be built. The full list of city names can be found in the commuting matrix
#'     (see `comm_matrix_cities_km`)
#' @param baseline_met_week,scenario_met_week Weekly MET spent performing leisure physical activity. They can be vectors
#' @param mode_from,mode_to Transportation modes in the baseline and alternative scenarios (only needed for
#'     the final output tibble, not involved in the computations)
#'     and commuting (hours)
#' @param dimension Indicates whether the dose-response function refers to non-communicable diseases (`morbidity`) related to
#'     air pollution or all-cause mortality (`mortality`). Default value is `morbidity`.
dose_res_phy <- function(city = "Roma",
                         baseline_met_week = seq(5, 25, by = 5),
                         scenario_met_week = seq(9, 45, by = 9),
                         mode_from = "car",
                         mode_to = "bike",
                         dimension = "morbidity"
){

  if (!is.element(dimension, c("morbidity", "mortality")))
    stop("Please enter a valid value for the dimension parameter: mortality or morbidity")


  if (dimension == "morbidity") {


    data <- morbidity_rr[morbidity_rr$dimension == "phy_act" &
                                    !is.na(morbidity_rr$rr),]

  } else if (dimension == "mortality"){

    data <- mortality_rr[mortality_rr$dimension == "phy_act" &
                                   mortality_rr$mode == mode_to, ]

    baseline_met_day <- ifelse(
      baseline_met_week > rep(data$threshold, length(baseline_met_week)),
      rep(data$threshold / 7, length(baseline_met_week)),
      baseline_met_week / 7
    )

    scenario_met_day <- ifelse(
      scenario_met_week > rep(data$threshold, length(scenario_met_week)),
      rep(data$threshold / 7, length(scenario_met_week)),
      scenario_met_week / 7
    )

  }


  data$coeff <- purrr::pmap_dbl(
    list(data$rr, data$exposure, data$trans_of_exp),
    function(x, y, z) (x - 1)/((y/7)^z)
  )

  data <- data[, c("outcome", "coeff", "trans_of_exp")]

  data[[paste0(mode_from, "_met_day")]] <- list(baseline_met_week / 7)

  data[[paste0(mode_to, "_met_day")]] <- list(scenario_met_week / 7)

  data <- data %>%
    tidyr::unnest(dplyr::where(is.list))

  data[[paste0("rr_", mode_from, "_to_", mode_to)]] <- purrr::pmap_dbl(
    list(
      data$coeff,
      data$trans_of_exp,
      data[[paste0(mode_from, "_met_day")]],
      data[[paste0(mode_to, "_met_day")]]
    ),
    function(x, y, z, g) {
      x*(z^y - g^y)
    }
  )

  data %>%
    dplyr::select("outcome", paste0("rr_", mode_from, "_to_", mode_to))

}
