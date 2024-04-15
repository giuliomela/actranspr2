#' Provide the average PM2.5 concentration for Italian municipalities
#'
#' This function returns the average (2010-2019) PM2.5 background concentration for Italian province capitals.
#' Data are from the WHO Air Pollution Database (2022 version). If data at city level are not available,
#' the function returns the average value of all cities in the same region.
#'
#' @param place A string character. The name (in Italian) of provice capital of interest.
#' @return A numeric value: the average PM2.5 background concentration expressed in um/m3.
pm25_concentration <- function(place = "Roma"){

  if(!is.element(place, unique(actranspr2::regional_capitals$prov_capital)))
    stop("Please provide a valid Italian province capital name.")

  if(place %in% air_pollution_ita$city) {

    air_pollution_ita[air_pollution_ita$city == place, ]$pm25

  } else {

    region_place <- regional_capitals %>%
      dplyr::filter(.data$city == place) %>%
      dplyr::pull(.data$region)

    air_pollution_ita %>%
      dplyr::left_join(regional_capitals) %>%
      dplyr::filter(.data$region == region_place) %>%
      dplyr::pull(.data$pm25) %>%
      mean(na.rm = T)

  }

}
