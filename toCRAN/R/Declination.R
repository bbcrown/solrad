#' Declination Angle
#'
#' This function calculates solar declination angle for a given day of year.
#'
#' @param DOY Day of year
#' @keywords Solar Declination Angle
#' @export
#' @examples
#'
#' #Calculating solar declination angle for 365 day of the year
#'
#' DOY <- 1:365
#'
#' delta <- Declination(DOY)
#'
#' plot(DOY, delta)
#'

Declination <- function(DOY){
  Delta <-   23.45*sin(pi/180*360/365 * (284 + DOY) )
  Delta
}
