#' Calculating Solar Variables
#'
#' This function calculates solar variables including radiation components, solar angles and positions and day length.
#' @param DOY Day of year
#' @param Lat Latitude (in degrees)
#' @param Lon Longitude in degrees
#' @param SLon Standard longitude (based on time zone) in degrees
#' @param DS Daylight saving in minutes
#' @param Elevation Elevation of the site in meters
#' @param Slope Site slope in degrees
#' @param Aspect Site aspect with respect to the south in degrees
#' @keywords Solar Variables
#' @export
#' @examples
#'
#' #Calculating solar variables angle for two consecutive days on 45 degree latitude and 10 degree longitude
#'
#' DOY <- seq(0, 2, .05)
#'
#' solar <- Incidence(Solar, Lat = 45, Lon=10, SLon=10, DS=0, Slope = 10, Aspect = 0)
#' #Note: only the difference between Lon and SLon matters not each value
#'
#' par(mfrow=c(3,1))
#' plot(DOY, solar$Altitude, ylim = c(-90,90))
#' plot(DOY, solar$Azimuth, col= 'red')
#'
#' plot(DOY, solar$Sdiropen)
#' lines(DOY, solar$Sdifopen, col='red')
#'

Solar <- function(DOY, Lat, Lon, SLon, DS, Elevation, Slope, Aspect){

  #calculate solar radiation and related variables based on location, time and topographical conditions
  DOY <- (DOY+365)%%365

  Delta <-   Declination(DOY)
  H  <-  HourAngle(DOY, Lon, SLon, DS)
  Alpha  <-   Altitude(DOY, Lat, Lon, SLon, DS)
  Hss <- Sunset(DOY, Lat)
  Hsr <- Sunrise(DOY, Lat)
  DL  <-  DayLength(DOY, Lat)

  Az <- Azimuth(DOY, Lat, Lon, SLon, DS)
  Theta  <-  Incidence(DOY, Lat, Lon, SLon, DS, Slope, Aspect)

  tb <- Transmittance(DOY, Lat, Lon, SLon, DS, Elevation)
  td <- DiffusionFactor(DOY, Lat, Lon, SLon, DS, Elevation)

  Sextr <- Extraterrestrial(DOY)
  SextrNormal <- ExtraterrestrialNormal(DOY, Lat, Lon, SLon, DS, Slope, Aspect)

  Sopen <- OpenRadiation(DOY, Lat, Lon, SLon, DS, Elevation)

  Sdiropen <- DirectRadiation(DOY, Lat, Lon, SLon, DS, Elevation, Slope, Aspect)
  Sdifopen <- DiffuseRadiation(DOY, Lat, Lon, SLon, DS, Elevation, Slope)

  list(Declination = Delta,
       Daylength=DL,
       Sunset = Hss,
       Sunrise = Hsr,
       Declination = Delta,
       Altitude=Alpha,
       Azimuth=Az,
       Incidence=Theta,
       Sextr = Sextr,
       Sopen = Sopen,
       Sdiropen=Sdiropen,
       Sdifopen=Sdifopen)
}
