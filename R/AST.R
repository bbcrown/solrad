#' Apparent Solar Time
#'
#' This function returns the apparent solar time (in minutes) for a given day of year and location.
#' @param DOY Day of year
#' @param Lon Longitude in degrees
#' @param SLon Standard longitude (based on time zone) in degrees
#' @param DS Daylight saving in minutes
#' @keywords  AST
#' @export
#' @examples
#'
#' #Calculating apparent solar time for two consecutive days
#'
#' DOY <- seq(0, 2, .05)
#'
#' ast <- AST(DOY, Lon=0, SLon=0, DS=60)
#' #Note: only the difference between Lon and SLon matters not each value
#'
#' plot(DOY, ast)
#'

AST  <-  function(DOY, Lon, SLon, DS){
  eot <- EOT(DOY)
  lst  <-  LST(DOY)
  ast <- lst + eot + 4* (SLon - Lon) - DS
  ast
}
