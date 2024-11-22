% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dose_res_phy.R
\name{dose_res_phy}
\alias{dose_res_phy}
\title{Dose-response function - physical activity}
\usage{
dose_res_phy(
  city = "Roma",
  baseline_met_week = seq(5, 25, by = 5),
  scenario_met_week = seq(9, 45, by = 9),
  mode_from = "car",
  mode_to = "bike",
  dimension = "morbidity"
)
}
\arguments{
\item{city}{A single character string. The name (in Italian) of the Italian municipality for which the
scenario must be built. The full list of city names can be found in the commuting matrix
(see `comm_matrix_cities_km`)}

\item{baseline_met_week, scenario_met_week}{Weekly MET spent performing leisure physical activity. They can be vectors}

\item{mode_from, mode_to}{Transportation modes in the baseline and alternative scenarios (only needed for
the final output tibble, not involved in the computations)
and commuting (hours)}

\item{dimension}{Indicates whether the dose-response function refers to non-communicable diseases (`morbidity`) related to
air pollution or all-cause mortality (`mortality`). Default value is `morbidity`.}
}
\description{
This function computes dose-response functions for physical activity . Such function
relates exposure to physical activity to health outcomes (namely incidence of some
non-communicable diseases). The output is a tibble in which the `rr_modefrom_to_modeto` column is the
coefficient to be used to calculate avoided cases from "general population" incidence.
}
