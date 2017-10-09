#' Equation of time
#'
#' This function approximates the value of equation of time for a given day of year
#' @param DOY Day of year
#' @keywords Equation of time value
#' @export
#' @examples
#'
#' #Calculating equaiton of time for 365 day of the year
#'
#' DOY <- 1:365
#'
#' eot <- EOT(DOY)
#'
#' plot(DOY, eot)
#'

EOT <- function(DOY){
  B  <-  (DOY - 81)*360/365
  ET <- 9.87*sin(pi/180*2*B)- 7.53*cos(pi/180*B)-1.5*sin(pi/180*B)
  ET
}
