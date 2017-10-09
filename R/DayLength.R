#' Day Length
#'
#' This function estimates day length (in hours) for a given day of year and latitude.
#' @param DOY Day of year
#' @param Lat Latitude (in degrees)
#' @keywords Day Length
#' @export
#' @examples
#'
#' #Calculating day length for 365 day of the year for 45 degree latitude
#'
#' DOY <- 1:365
#'
#' Lat = 45
#'
#' dl <- DayLength(DOY, Lat)
#'
#' plot(DOY, dl)
#'

DayLength <- function(DOY, Lat){
  DL  <-  Sunset(DOY, Lat) - Sunrise(DOY, Lat)
  DL
}
