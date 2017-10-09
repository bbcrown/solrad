#' Solar Altitude Angle
#'
#' This function solar altitude angle (in degrees) for a given day of year and location.
#' @param DOY Day of year
#' @param Lat Latitude in degrees
#' @param Lon Longitude in degrees
#' @param SLon Standard longitude (based on time zone) in degrees
#' @param DS Daylight saving in minutes
#' @keywords Altitude
#' @export
#' @examples
#'
#' #Calculating solar altitude angle for two consecutive days
#'
#' DOY <- seq(0, 2, .05)
#'
#' alpha <- Altitude(DOY, Lat = 45, Lon=0, SLon=0, DS=60)
#' #Note: only the difference between Lon and SLon matters not each value
#'
#' plot(DOY, alpha)

Altitude  <-   function(DOY, Lat, Lon, SLon, DS){
  Delta <-   Declination(DOY)
  H  <-  HourAngle(DOY, Lon, SLon, DS)
  A <- 180/pi*asin(sin(pi/180*Lat)*sin(pi/180*Delta)+
                     cos(pi/180*Lat)*cos(pi/180*Delta)*cos(pi/180*H))
  A
}
