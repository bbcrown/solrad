#' Sunset Time
#'
#' This function estimates sunset time (in continuous hour values) for a given day of year and latitude.
#' @param DOY Day of year
#' @param Lat Latitude (in degrees)
#' @keywords Sunset
#' @export
#' @examples
#'
#' #Calculating sunset time for 365 day of the year for 45 degree latitude
#'
#' DOY <- 1:365
#'
#' Lat = 45
#'
#' sunset <- Sunset(DOY, Lat)
#'
#' plot(DOY, sunset)
#'

Sunset <- function(DOY, Lat){
  Delta <-   Declination(DOY)
  Hss  <-  180/pi*acos(-tan(pi/180*Lat)*tan(pi/180*Delta))/15
  Hss
}
