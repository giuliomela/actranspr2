% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/inhaled_dose.R
\name{inhaled_dose}
\alias{inhaled_dose}
\title{Compute the inhaled dose of air pollutants by individuals}
\usage{
inhaled_dose(
  city = "Roma",
  mode = "car",
  w_phy_act = 5,
  w_commuting = 4,
  experimental_data = FALSE,
  working_weeks = 48,
  commuting_days = 4,
  eq_change = TRUE
)
}
\arguments{
\item{city}{A single character string. The name (in Italian) of the Italian muncipality for which the
scenario must be built. THe full list of city names can be found in the commuting matrix
(see `comm_matrix_cities_km`)}

\item{mode}{A character string. The transportation mode of interest:  it can assume the following values:
`walk`, `bike` and `ebike`.}

\item{w_phy_act}{The weekly hours of physical activity.}

\item{w_commuting}{The weekly hours of commuting.}

\item{experimental_data}{A logical value. If set to `TRUE` data from the RSE experimentation are used.
Default set to `FALSE`}

\item{working_weeks}{Number of annual working weeks}

\item{commuting_days}{Number of weekly commuting days. Default is `4`}

\item{eq_change}{whether the function returns the equivalent change or not}
}
\value{
Either a vector or a number depending on the `ratio` parameter
}
\description{
This function computes the daily inhaled dose of PM2.5 for individuals living in
different Italian municipalities. The function takes into account the commuting mode and
the weekly hours of commuting and physical activity. Total inhaled doses are calculated following Engström and Forsberg (2019). It can return the ration between the inhaled dose
in the commuting scenario (i.e. by car or bike) and a baseline scenario (no commuting at all).
}
