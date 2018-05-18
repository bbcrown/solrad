#' Solar Hour Angle
#'
#' This function returns solar hour angle for a given day of year, and location.
#' @param DOY Day of year
#' @param Lon Longitude in degrees
#' @param SLon Standard longitude (based on time zone) in degrees
#' @param DS Daylight saving in minutes
#' @keywords Hour Angle
#' @export
#' @examples
#'
#' #Calculating solar hour angle for two consecutive days
#'
#' DOY <- seq(0, 2, .05)
#'
#' h <- HourAngle(DOY, Lon=0, SLon=0, DS=60)
#' #Note: only the difference between Lon and SLon matters not each value
#'
#' plot(DOY, h)
#'

HourAngle  <-  function(DOY, Lon, SLon, DS){
  AST  <-  AST(DOY, Lon, SLon, DS)
  H <- (AST - 12*60)/4
  H
}
