# install.packages("devtools")
library("devtools")
# devtools::install_github("klutometis/roxygen")
# library(roxygen2)

# create('~/Projects/calcSolar/')
document('~/Projects/calcSolar/')
install('~/Projects/calcSolar/')




#' Declination Angle
#'
#' This function calculates solar declination angle for a given day of year.
#' @param DOY Day of year
#' @keywords Solar Declination Angle
#' @export
#' @examples
#' #Calculating solar declination angle for 365 day of the year
#' DOY <- 1:365
#' delta <- Declination(DOY)
#' plot(DOY, delta)
