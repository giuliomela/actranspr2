#' Compute the alive population given a starting population of a vector of mortality rates
#'
#' @param starting_pop A numeric value. The starting population.
#' @param death_rate A numeric vector. Annual mortality rates.
#' @return A numeric vector
alive_pop <- function(starting_pop, death_rate){

  # computing alive population given a starting population and a specific death rate

  purrr::accumulate(death_rate[2:length(death_rate)], ~{.x * (1 - .y)},
                    .init = starting_pop * (1 - death_rate[1]))


}
