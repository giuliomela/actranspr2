#' Compute the (discounted) amount of years lost to disability
#'
#' This function computes the years lost to disability (YLD) taking into account disease-specific
#' disability weights provided, a country-specific social discount rate and the average disease length.
#' Such parameters are provided by the user.
#'
#' @param acute_dw,chronic_dw Acute (first year) and chronic disability weights
#' @param sdr The social discount rate
#' @param disease_length Average disease length on average
#' @return The discounted amount of years lost to disability
compute_yld <- function(acute_dw = 0.5,
                        chronic_dw = 0.1,
                        sdr = 0.02,
                        disease_length = 25){

  round_disease_length <- round(disease_length)

  if (round_disease_length > 1) {


    years <- 0:(round_disease_length - 1)
    dw <- c(acute_dw, rep(chronic_dw, round_disease_length - 1))
    disc_fct <- sapply(years, function(x) (1 + sdr)^-x)

    sum(dw * disc_fct)

  } else if (round_disease_length <= 1) {

    acute_dw * disease_length

  }

}
