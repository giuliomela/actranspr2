#' Dose-response functions - air pollution
#'
#' This function computes dose-response functions for air pollution. Such function
#' relates exposure to air pollution (PM2.5) to health outcomes (namely incidence of some
#' non-communicable diseases and all-cause mortality).
#'
#' @param city A single character string. The name (in Italian) of the Italian municipality for which the
#'     scenario must be built. THe full list of city names can be found in the commuting matrix
#'     (see `comm_matrix_cities_km`)
#' @param mode_from,mode_to Transportation modes in the baseline and alternative scenarios (only needed for
#'     the final output tibble, not involved in the computations)
#'     and commuting (hours)
#' @param w_phy_act,w_commuting_from,w_commuting_to Weekly time spent performing leisure physical activity,
#'     and commuting (both in the original and active mobility scenarios).
#' @param experimental_data Indicates whether experimental data or data from the literature should be used to
#'     estimate the ventilation rate of `mode_to`.
#' @param commuting_days Number of weekly commuting days. Default is `4`
#' @param working_weeks Number of annual working weeks
#' @param dimension Indicates whether the dose-response function refers to non-communicable diseases (`morbidity`) related to
#'     air pollution or all-cause mortality (`mortality`). Default value is `morbidity`.
#' @importFrom rlang .data
dose_res_air <- function(city = "Roma",
                         mode_from = "car",
                         mode_to = "bike",
                         w_phy_act = c(5, 7, 8, 2, 1),
                         w_commuting_from = seq(4, 20, by = 4),
                         w_commuting_to = seq(6, 30, by = 6),
                         experimental_data = FALSE,
                         working_weeks = 48,
                         commuting_days = 4,
                         dimension = "morbidity"
){

  if (!is.element(dimension, c("morbidity", "mortality")))
    stop("Please enter a valid value for the dimension parameter: mortality or morbidity")

  if(length(w_phy_act) != length(w_commuting_from))
    stop("w_phy_act and w_commuting_from must have the same length")

  if (dimension == "morbidity"){

    data <- morbidity_rr %>%
      dplyr::filter(dimension == "air_pollution") %>%
      dplyr::select(!"trans_of_exp") %>%
      tidyr::drop_na()

  } else if (dimension == "mortality") {

    data <- mortality_rr %>%
      dplyr::filter(mode == mode_to,
                    dimension == "air_pollution")

  }

  mode_from_rep <- rep(mode_from, length(w_commuting_from))

  mode_to_rep <- rep(mode_to, length(w_commuting_to))

  eq_change_from <- purrr::pmap_dbl(
    list(
      mode_from_rep,
      w_phy_act,
      w_commuting_from
    ),
    function(x, y, z){
      inhaled_dose(
        city = {{city}},
        mode = x,
        w_phy_act = y,
        w_commuting = z,
        experimental_data = F,
        working_weeks = {{working_weeks}},
        commuting_days = {{commuting_days}}
      )
    }
  )


  eq_change_to <- purrr::pmap_dbl(
    list(
      mode_to_rep,
      w_phy_act,
      w_commuting_to
    ),
    function(x, y, z){
      inhaled_dose(
        city = {{city}},
        mode = x,
        w_phy_act = y,
        w_commuting = z,
        experimental_data = experimental_data,
        working_weeks = working_weeks,
        commuting_days = commuting_days
      )
    }
  )

  eq_change <- list(eq_change_from, eq_change_to)

  names(eq_change) <- c(mode_from, mode_to)

  data <- data %>%
    dplyr::slice(rep(1:dplyr::n(), each = length(w_commuting_from)))

  for (i in c(mode_from, mode_to)){

    data[[paste0("eq_change_", i)]] <- rep(eq_change[[i]],
                                           times = nrow(data) / length(w_commuting_from))

  }



  for (i in c(mode_from, mode_to)) {

    data[[paste0("rr_", i)]] <- purrr::pmap_dbl(
      list(
        data$rr,
        data$exposure,
        data[[paste0("eq_change_", i)]]
      ),
      function(x, y, z)
        exp(log(x) * (z / y))
    )

  }


  data[[paste0("rr_", mode_from, "_to_", mode_to)]] <- (data[[paste0("rr_", mode_from)]] - data[[paste0("rr_", mode_to)]]) * -1

  data[, c("outcome", paste0("rr_", mode_from, "_to_", mode_to))]


}
