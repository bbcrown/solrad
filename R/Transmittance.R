#' Atmospheric Transmittance
#'
#' This function returns atmospheric transmittance coefficient for a given day of year and location.
#' @param DOY Day of year
#' @param Lat Latitude (in degrees)
#' @param Lon Longitude in degrees
#' @param SLon Standard longitude (based on time zone) in degrees
#' @param DS Daylight saving in minutes
#' @param Elevation Elevation of the site in meters
#' @keywords  Atmospheric Transmittance
#' @export
#' @examples
#'
#' #Calculating atmospheric transmittance coefficient for two consecutive days on 45 degree latitude and 10 degree longitude and at 100 m altitude.
#'
#' DOY <- seq(0, 2, .05)
#'
#' tb <- Transmittance(DOY, Lat = 45, Lon=10, SLon=10, DS=0, Elevation = 100)
#'
#' #Note: only the difference between Lon and SLon matters not each value
#'
#' plot(DOY, tb)
#'

Transmittance <- function(DOY, Lat, Lon, SLon, DS, Elevation){
  a0 <- 0.4237-0.00821*(6-Elevation/1000.0)^2
  a1 <- 0.5055+0.00595*(6.5-Elevation/1000.0)^2
  k <- 0.2711+0.01858*(2.5-Elevation/1000.0)^2
  A  <-   Altitude(DOY, Lat, Lon, SLon, DS)
  Alpha  <-   Altitude(DOY, Lat, Lon, SLon, DS)

  tb <- (a0+a1*exp(-k/sin(pi/180*Alpha)))*(Alpha>0)
  tb
}



#Calculating atmospheric transmittance coefficient for two consecutive days on 45 degree latitude and 10 degree longitude and at 100 m altitude.

DOY <- seq(0, 2, .05)

tb <- Transmittance(DOY, Lat = 45, Lon=10, SLon=10, DS=0, Elevation = 100)

#Note: only the difference between Lon and SLon matters not each value

plot(DOY, tb)
