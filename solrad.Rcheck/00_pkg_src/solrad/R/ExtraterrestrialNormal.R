#' Normal Extraterrestrial Solar Radiation
#'
#' This function calculates extraterrestrial solar radiation normal to surface (in W/m2) for a given day of year, location and topogrpahy.
#' @param DOY Day of year
#' @param Lat Latitude (in degrees)
#' @param Lon Longitude in degrees
#' @param SLon Standard longitude (based on time zone) in degrees
#' @param DS Daylight saving in minutes
#' @param Slope Site slope in degrees
#' @param Aspect Site aspect with respect to the south in degrees
#' @keywords  Normal Extraterrestrial Solar Radiation
#' @export
#' @examples
#'
#' #Calculating solar incidence angle for two consecutive days on 45 degree latitude and
#' # 10 degree longitude
#'
#' DOY <- seq(0, 2, .05)
#'
#' SextrNormal <- ExtraterrestrialNormal(DOY, Lat = 45, Lon=10, SLon=10, DS=0, Slope = 10, Aspect = 0)
#' #Note: only the difference between Lon and SLon matters not each value
#'
#' plot(DOY, SextrNormal)
#'

ExtraterrestrialNormal <- function(DOY, Lat, Lon, SLon, DS, Slope, Aspect){
  Theta  <-  Incidence(DOY, Lat, Lon, SLon, DS, Slope, Aspect)
  Sextr <- Extraterrestrial(DOY)
  SextrNormal <- Sextr*cos(pi/180*Theta)
  SextrNormal[SextrNormal<0] <- 0
  SextrNormal
}
