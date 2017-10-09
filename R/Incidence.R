#' Solar Incidence Angle
#'
#' This function returns solar incidence angle (in degrees) for a given day of year and location and site slope and aspect. The solar incidence angle is the angle between sun's ray and the normal on a surface.
#' @param DOY Day of year
#' @param Lat Latitude (in degrees)
#' @param Lon Longitude in degrees
#' @param SLon Standard longitude (based on time zone) in degrees
#' @param DS Daylight saving in minutes
#' @param Slope Site slope in degrees
#' @param Aspect Site aspect with respect to the south in degrees
#' @keywords  Solar Incidence Angle
#' @export
#' @examples
#'
#' #Calculating solar incidence angle for two consecutive days on 45 degree latitude and 10 degree longitude
#'
#' DOY <- seq(0, 2, .05)
#'
#' theta <- Incidence(DOY, Lat = 45, Lon=10, SLon=10, DS=0, Slope = 10, Aspect = 0)
#' #Note: only the difference between Lon and SLon matters not each value
#'
#' plot(DOY, theta)
#'

Incidence  <-  function(DOY, Lat, Lon, SLon, DS, Slope, Aspect){
  Delta <-   Declination(DOY)
  H  <-  HourAngle(DOY, Lon, SLon, DS)

  theta <-180/pi*acos(sin(pi/180*Lat)*sin(pi/180*Delta)*cos(pi/180*Slope) -
                        cos(pi/180*Lat)*sin(pi/180*Delta)* sin(pi/180*Slope)* cos(pi/180*Aspect) +
                        cos(pi/180*Lat)*cos(pi/180*Delta)* cos(pi/180*H)*cos(pi/180*Slope)+
                        sin(pi/180*Lat)*cos(pi/180*Delta)* cos(pi/180*H)*sin(pi/180*Slope)*cos(pi/180*Aspect)+
                        cos(pi/180*Delta)*sin(pi/180*H)*sin(pi/180*Slope)*sin(pi/180*Aspect)        )
  theta
}
