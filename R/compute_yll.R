#' Compute the discounted number of years of life lost due ti premature death
#'
#' This function computes the years of life lost (YLL) taking into disease length (to establish the
#' discounting factor), a user-defined social discount rate and the reduction in expected reduction
#' in life expectancy,
#'
#' @param life_exp_red Reduction in life expectancy due to disease (years)
#' @param sdr The social discount rate
#' @param disease_length Average disease length on average
#' @return The discounted amount of years of life lost
compute_yll <- function(disease_length = 15,
                        life_exp_red = 20,
                        sdr = 0.02){

  life_exp_red / (1 + sdr)^disease_length


}
