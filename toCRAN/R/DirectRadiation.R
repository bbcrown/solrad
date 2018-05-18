#' Solar Direct Beam Radiation on Surface
#'
#' This function returns solar open direct beam dadiation (in W/m2) for a given day of year, location and topography.
#' @param DOY Day of year
#' @param Lat Latitude (in degrees)
#' @param Lon Longitude in degrees
#' @param SLon Standard longitude (based on time zone) in degrees
#' @param DS Daylight saving in minutes
#' @param Elevation Elevation of the site in meters
#' @param Slope Site slope in degrees
#' @param Aspect Site aspect with respect to the south in degrees
#' @keywords  Direct Beam Radiation
#' @export
#' @examples
#'
#' #Calculating atmospheric transmittance coefficient for two consecutive days on 45 degree
#' #latitude and 10 degree longitude and at 100 m altitude.
#'
#' DOY <- seq(0, 2, .05)
#'
#' Sopen <- OpenRadiation(DOY, Lat = 45, Lon=10, SLon=10, DS=0, Elevation = 100)
#' #Note: only the difference between Lon and SLon matters not each value
#'
#' plot(DOY, Sopen)
#'

DirectRadiation <- function(DOY, Lat, Lon, SLon, DS, Elevation, Slope, Aspect){
  Theta  <-  Incidence(DOY, Lat, Lon, SLon, DS, Slope, Aspect)
  Sopen <- OpenRadiation(DOY, Lat, Lon, SLon, DS, Elevation)
  Sdiropen <- Sopen*cos(pi/180*Theta)
  Sdiropen
}
