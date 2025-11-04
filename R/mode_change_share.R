#' Return the share of commuters shifting from passive to active mobility
#'
#' @param km_one_way A numeric value. The distance (in km) commuters have to travel every day (one way only)
#' @param share.less_3km A numeric value. The share of commuters commuting less than 3 km (one way)
#'     making the modal switch.
#' @param share.3_10km A numeric value. The share of commuters commuting between 3 and 10 km (one way)
#'     making the modal switch.
#' @param share.10_15km A numeric value. The share of commuters commuting between 10 and 15 km (one way)
#'     making the modal switch.
#' @param share.more_15km A numeric value. The share of commuters commuting more than 15 km (one way)
#'     making the modal switch.
#' @return A numeric value.
mode_change_share <- function(km_one_way,
                              share.less_3km = 0.3, share.3_10km = 0.2, share.10_15km = 0,
                              share.more_15km = 0){


  if((share.less_3km | share.3_10km | share.10_15km | share.more_15km) > 1 |
     (share.less_3km | share.3_10km | share.10_15km | share.more_15km) < 0)
    stop("The 'share' parameters must all be between 0 and 1")

  # if(sum(share.less_3km, share.3_10km, share.10_15km, share.more_15km) > 1)
  #   stop("The sum of commuter shares must be lower or equal to one")

  share <- dplyr::case_when(
    km_one_way < 3 ~ share.less_3km,
    km_one_way >= 3 & km_one_way < 10 ~ share.3_10km,
    km_one_way >= 10 & km_one_way < 15 ~ share.10_15km,
    TRUE ~ share.more_15km
  )

  share

}
