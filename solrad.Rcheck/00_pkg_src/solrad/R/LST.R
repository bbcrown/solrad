#' Local Standard Time
#'
#' This function returns local standard time (in minutes) given a day of the year value.
#' @param DOY Day of year
#' @keywords LST
#' @export
#' @examples
#'
#' #Calculating  local standard time for two consecutive days
#'
#' DOY <- seq(0, 2, .05)
#'
#' lst <- LST(DOY)
#'
#' plot(DOY, lst)
#'

LST <- function(DOY){
  lst  <-  (DOY*24*60)%%(24*60)
  lst
}
