#' Sunrise Time
#'
#' This function estimates sunrise time (in continuous hour values) for a given day of year and latitude.
#' @param DOY Day of year
#' @param Lat Latitude (in degrees)
#' @keywords Sunrise
#' @export
#' @examples
#'
#' #Calculating sunrise time for 365 day of the year for 45 degree latitude
#'
#' DOY <- 1:365
#'
#' Lat = 45
#'
#' sunrise <- Sunset(DOY, Lat)
#'
#' plot(DOY, sunrise)
#'

Sunrise <- function(DOY, Lat){
  Delta <-   Declination(DOY)
  Hsr  <-  -180/pi*acos(-tan(pi/180*Lat)* tan(pi/180*Delta))/15
  Hsr
}
