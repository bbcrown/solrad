#' Solar Extraterrestrial Radiation
#'
#' This function calculates solar extraterrestrial radiation (in W/m2) for a given day of year.
#' @param DOY Day of year
#' @keywords Solar Extraterrestrial Radiation
#' @export
#' @examples
#'
#' #Calculating solar extraterrestrial radiation for 365 day of the year
#'
#' DOY <- 1:365
#'
#' Sextr <- Extraterrestrial(DOY)
#'
#' plot(DOY, Sextr)
#'

Extraterrestrial <- function(DOY){
  Sextr <- SolarConstant*(1+0.033*cos(pi/180*360*DOY/365))
  Sextr
}
