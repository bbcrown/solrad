#' Atmospheric Diffusion Factor
#'
#' This function returns atmospheric diffusion factor for a given day of year, location and topography.
#' @param DOY Day of year
#' @param Lat Latitude (in degrees)
#' @param Lon Longitude in degrees
#' @param SLon Standard longitude (based on time zone) in degrees
#' @param DS Daylight saving in minutes
#' @param Elevation Elevation of the site in meters
#' @keywords  Atmospheric Diffusion
#' @export
#' @examples
#'
#' #Calculating atmospheric diffusion factor for two consecutive days on 45 degree latitude and 10 degree longitude and at 100 m altitude.
#'
#' DOY <- seq(0, 2, .05)
#'
#' td <- DiffusionFactor(DOY, Lat = 45, Lon=10, SLon=10, DS=0, Elevation = 100)
#' #Note: only the difference between Lon and SLon matters not each value
#'
#' plot(DOY, td)
#'

DiffusionFactor <- function(DOY, Lat, Lon, SLon, DS, Elevation){
  tb <- Transmittance(DOY, Lat, Lon, SLon, DS, Elevation)
  td <- 0.271-0.294*tb
  td
}
