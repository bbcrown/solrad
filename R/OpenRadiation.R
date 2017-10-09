#' Open Sky Solar Radiation
#'
#' This function returns open sky solar radiation (in W/m2) for a given day of year and location.
#' @param DOY Day of year
#' @param Lat Latitude (in degrees)
#' @param Lon Longitude in degrees
#' @param SLon Standard longitude (based on time zone) in degrees
#' @param DS Daylight saving in minutes
#' @param Elevation Elevation of the site in meters
#' @keywords  Open Radiation
#' @export
#' @examples
#'
#' #Calculating open sky solar radiation for two consecutive days on 45 degree latitude and 10 degree longitude and at 100 m altitude.
#'
#' DOY <- seq(0, 2, .05)
#'
#' Sopen <- OpenRadiation(DOY, Lat = 45, Lon=10, SLon=10, DS=0, Elevation = 100)
#' #Note: only the difference between Lon and SLon matters not each value
#'
#' plot(DOY, Sopen)

OpenRadiation <- function(DOY, Lat, Lon, SLon, DS, Elevation){
  tb <- Transmittance(DOY, Lat, Lon, SLon, DS, Elevation)
  Sextr <- Extraterrestrial(DOY)

  Sopen <- tb*Sextr
  Sopen
}
