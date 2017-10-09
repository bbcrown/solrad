#' Solar Azimuth Angle
#'
#' This function returns solar azimuth angle (in degrees) for a given day of year and location. The solar azimuth angle is the angle of sun's ray measured in the horizental plane from due south
#' @param DOY Day of year
#' @param Lat Latitude (in degrees)
#' @param Lon Longitude in degrees
#' @param SLon Standard longitude (based on time zone) in degrees
#' @param DS Daylight saving in minutes
#' @keywords  Azimuth
#' @export
#' @examples
#'
#' #Calculating solar azimuth angle for two consecutive days on 45 degree latitude and 10 degree longitude
#'
#' DOY <- seq(0, 2, .05)
#'
#' Az <- Azimuth(DOY, Lat = 45, Lon=10, SLon=10, DS=0)
#' #Note: only the difference between Lon and SLon matters not each value
#'
#' plot(DOY, Az)
#'

Azimuth <- function(DOY, Lat, Lon, SLon, DS){

  delta <-   Declination(DOY)
  H  <-  HourAngle(DOY, Lon, SLon, DS)
  ast  <-  AST(DOY, Lon, SLon, DS)
  alpha  <-   Altitude(DOY, Lat, Lon, SLon, DS)

  rhs <- cos(pi/180*delta)*
    sin(pi/180*H)/
    cos(pi/180*alpha)

  Azimuth1  <-  180/pi*asin(rhs)

  c1 <- (ast < 12*60)*1
  c2 <- (cos(pi/180*H)> tan(pi/180*delta)/tan(pi/180*Lat))*1

  Azimuth2 <-
    c1*(-180 + abs(Azimuth1)) +
    (1-c1)*(180 - Azimuth1)

  Az <-
    c2*Azimuth1 +
    (1-c2)*Azimuth2

  Az
}


