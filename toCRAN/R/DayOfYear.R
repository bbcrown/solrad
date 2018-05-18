#' Day of year
#'
#' This function returns a continuous the day of year value (as integer value 1:365) for a given date-time in "POSIXlt" "POSIXct" format.
#' @param DateTime DateTime object
#' @keywords DOY, Day of year
#' @export
#' @examples
#'
#' #Calculating day of year for now
#'
#' DayOfYear(Sys.time())
#'

DayOfYear <- function(DateTime){
  DOY <- as.numeric(strftime(DateTime, format = "%j"))
  hours <- as.numeric(strftime(DateTime, format="%H"))
  mins <- as.numeric(strftime(DateTime, format="%M"))
  secs <- as.numeric(strftime(DateTime, format="%S"))
  DOY <- DOY + hours/24 + mins/24/60 + secs/24/60/60
  DOY
}
