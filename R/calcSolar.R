DayOfYear <- function(Date){
  DOY <- as.numeric(strftime(Date, format = "%j"))
  DOY
}


#' Equation of time
#'
#' This function approximate the value of equation of time for a given date.
#' @param DOY Day of year
#' @keywords Equation of time value
#' @export
#' @examples
#' DOY <- 1:365
#' e <- eot(DOY)
#' plot(DOY, e)

eot <- function(DOY){
  B  <-  (DOY - 81)*360/365
  ET <- 9.87*sin(pi/180*2*B)- 7.53*cos(pi/180*B)-1.5*sin(pi/180*B)
  ET
}

Declination <- function(DOY){
  Delta <-   23.45*sin(pi/180*360/365 * (284 + DOY) )
  Delta
}

LST <- function(DOY){
  LST  <-  (DOY*24*60)%%(24*60)
  LST
}

AST  <-  function(DOY, Lon, SLon, DS){
  ET <- eot(DOY)
  LST  <-  (DOY*24*60)%%(24*60)
  AST <- LST + ET + 4* (SLon - Lon) - DS
  AST
}

HourAngle  <-  function(DOY, Lon, SLon, DS){
  AST  <-  AST(DOY, Lon, SLon, DS)
  H <- (AST - 12*60)/4
  H
}

Altitude  <-   function(DOY, Lat, Lon, SLon, DS){
  Delta <-   Declination(DOY)
  H  <-  HourAngle(DOY, Lon, SLon, DS)
  A <- 180/pi*asin(sin(pi/180*Lat)*sin(pi/180*Delta)+
                     cos(pi/180*Lat)*cos(pi/180*Delta)*cos(pi/180*H))
  A
}

Sunset <- function(DOY, Lat){
  Delta <-   Declination(DOY)
  Hss  <-  180/pi*acos(-tan(pi/180*Lat)*tan(pi/180*Delta))/15
  Hss
}

Sunrise <- function(DOY, Lat){
  Delta <-   Declination(DOY)
  Hsr  <-  -180/pi*acos(-tan(pi/180*Lat)* tan(pi/180*Delta))/15
  Hsr
}

DayLength <- function(DOY, Lat){
  DL  <-  Hss(DOY, Lat) - Hsr(DOY, Lat)
  DL
}

Azimuth <- function(DOY, Lat, Lon, SLon, DS){
  Delta <-   Declination(DOY)
  H  <-  HourAngle(DOY, Lon, SLon, DS)
  AST  <-  AST(DOY, Lon, SLon, DS)
  A  <-   Altitude(DOY, Lat, Lon, SLon, DS)

  #(*Solar Azimuth Angle*)
  #(*The angle of sun's ray measured in the horizental plane from due south*)
  Azimuth1  <-  180/pi*asin(cos(pi/180*Delta)*
                              sin(pi/180*H)/cos(pi/180*A))
  tmp <- AST < 12*60
  Azimuth2 <-
    (-180 + abs(Azimuth1))*tmp +    (Azimuth2 <- -180 + abs(Azimuth1))*(!tmp)

  tmp <- (cos(pi/180*H)> tan(pi/180*Delta)/tan(pi/180*Lat))

  Az <- tmp*Azimuth1+Azimuth2*(!tmp)
  Az
}

Transmittance <- function(DOY, Lat, Lon, SLon, DS, Elevation){
  a0 <- 0.4237-0.00821*(6-Elevation/1000.0)^2
  a1 <- 0.5055+0.00595*(6.5-Elevation/1000.0)^2
  k <- 0.2711+0.01858*(2.5-Elevation/1000.0)^2
  A  <-   Altitude(DOY, Lat, Lon, SLon, DS)

  tb <- (a0+a1*exp(-k/sin(pi/180*Alpha)))*(Alpha>0)
  tb
}

DiffusionFactor <- function(DOY, Lat, Lon, SLon, DS, Elevation){
  tb <- Transmittance(DOY, Lat, Lon, SLon, DS, Elevation)
  td <- 0.271-0.294*tb
  td
}

#(*Solar Incidence Angle*)
#(*The angle between sun's ray and the normal on a surface*)
Incidence  <-  function(DOY, Lat, Lon, SLon, DS, Slope, Aspect){
  Delta <-   Declination(DOY)
  H  <-  HourAngle(DOY, Lon, SLon, DS)

  theta <-180/pi*acos(sin(pi/180*Lat)*sin(pi/180*Delta)*cos(pi/180*Slope) -
                        cos(pi/180*Lat)*sin(pi/180*Delta)* sin(pi/180*Slope)* cos(pi/180*Aspect) +
                        cos(pi/180*Lat)*cos(pi/180*Delta)* cos(pi/180*H)*cos(pi/180*Slope)+
                        sin(pi/180*Lat)*cos(pi/180*Delta)* cos(pi/180*H)*sin(pi/180*Slope)*cos(pi/180*Aspect)+
                        cos(pi/180*Delta)*sin(pi/180*H)*sin(pi/180*Slope)*sin(pi/180*Aspect)        )
  theta
}

SolarConstant <- 1366.1

Extraterrestrial <- function(DOY){
  Sextr <- SolarConstant*(1+0.033*cos(pi/180*360*DOY/365))
  Sextr
}

ExtraterrestrialNormal <- function(DOY, Lat, Lon, SLon, DS, Slope, Aspect){
  Theta  <-  Incidence(DOY, Lat, Lon, SLon, DS, Slope, Aspect)
  Sextr <- Extraterrestrial(DOY)
  SextrNormal <- Sextr*cos(pi/180*Theta)
  SextrNormal
}

OpenRadiation <- function(DOY, Lat, Lon, SLon, DS, Elevation){
  tb <- Transmittance(DOY, Lat, Lon, SLon, DS, Elevation)
  Sextr <- Extraterrestrial(DOY)

  Sopen <- tb*Sextr
  Sopen
}

DirectRadiation <- function(DOY, Lat, Lon, SLon, DS, Elevation){
  Theta  <-  Incidence(DOY, Lat, Lon, SLon, DS, Slope, Aspect)
  Sopen <- OpenRadiation(DOY, Lat, Lon, SLon, DS, Elevation)
  Sdiropen <- Sopen*cos(pi/180*Theta)
  Sdiropen
}

DiffuseRadiation <- function(DOY, Lat, Lon, SLon, DS, Elevation){
  Alpha  <-   Altitude(DOY, Lat, Lon, SLon, DS)
  Sopen <- OpenRadiation(DOY, Lat, Lon, SLon, DS, Elevation)
  Sdifopen <- Sopen*td*(sin(pi/180*Alpha))^2*(cos(pi/180*Slope/2.))^2
  Sdifopen
}




Solar <- function(DOY, Lat, Slope=0, Aspect=0, Lon=0, SLon=0, DS=0, Elevation=0){

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

  Sdiropen <- DirectRadiation(DOY, Lat, Lon, SLon, DS, Elevation)
  Sdifopen <- DiffuseRadiation(DOY, Lat, Lon, SLon, DS, Elevation)

  list(Daylength=DayLength,
       Altitude=Alpha,
       Azimuth=Az,
       Incidence=Theta,
       Sdiropen=Sdiropen,
       Sdifopen=Sdifopen)
}
