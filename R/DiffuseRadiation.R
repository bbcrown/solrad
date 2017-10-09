#' Solar Diffuse Radiation on a Surface
#'
#' This function returns solar diffuse dadiation (in W/m2) for a given day of year, location and topography.
#' @param DOY Day of year
#' @param Lat Latitude (in degrees)
#' @param Lon Longitude in degrees
#' @param SLon Standard longitude (based on time zone) in degrees
#' @param DS Daylight saving in minutes
#' @param Elevation Elevation of the site in meters
#' @keywords  Diffuse Radiation
#' @export
#' @examples
#'
#' #Calculating atmospheric transmittance coefficient for two consecutive days on 45 degree latitude and 10 degree longitude and at 100 m altitude.
#'
#' DOY <- seq(0, 2, .05)
#'
#' Sdifopen <- DiffuseRadiation(DOY, Lat = 45, Lon=10, SLon=10, DS=0, Elevation = 100, Slope = 0)
#' #Note: only the difference between Lon and SLon matters not each value
#'
#' plot(DOY, Sdifopen)
#'

DiffuseRadiation <- function(DOY, Lat, Lon, SLon, DS = 0, Elevation, Slope = 0){
  Alpha  <-   Altitude(DOY, Lat, Lon, SLon, DS)
  Sopen <- OpenRadiation(DOY, Lat, Lon, SLon, DS, Elevation)
  td <- DiffusionFactor(DOY, Lat, Lon, SLon, DS, Elevation)

  Sdifopen <- Sopen*td*(sin(pi/180*Alpha))^2*(cos(pi/180*Slope/2.))^2
  Sdifopen
}

