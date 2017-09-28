calcSolar <- function(DOY, Lat, Slope=0, Aspect=0, Lon=0, SLon=0, DS=0, Elevation=0){
  
#calculate solar radiation and related variables based on location, time and topographical conditions 
  
  DOY <- (DOY+365)%%365
  
  B  <-  (DOY - 81)*360/365
  ET  <-  9.87*sin(pi/180*2*B)- 7.53*cos(pi/180*B)-1.5*sin(pi/180*B)
  Delta <-   23.45*sin(pi/180*360/365 * (284 + DOY) )
  LST  <-  (DOY*24*60)%%(24*60)
  AST  <-  LST + ET + 4* (SLon - Lon) - DS 
  HourAngle  <-  (AST - 12*60)/4
  Altitude  <-   180/pi*asin(sin(pi/180*Lat)*sin(pi/180*Delta)+ cos(pi/180*Lat)*cos(pi/180*Delta)*cos(pi/180*HourAngle))
  
  #(*Sunset Time*)
  Hss  <-  180/pi*acos(-tan(pi/180*Lat)*tan(pi/180*Delta))/15
  
  #(*Sunrise Time*)
  Hsr  <-  -180/pi*acos(-tan(pi/180*Lat)* tan(pi/180*Delta))/15
  
  #(*Day Length*)
  DayLength  <-  Hss - Hsr
  
  #(*Solar Azimuth Angle*)
  #(*The angle of sun's ray measured in the horizental plane from due south*)
  Azimuth1  <-  180/pi*asin(cos(pi/180*Delta)*
                              sin(pi/180*HourAngle)/cos(pi/180*Altitude))
  tmp <- AST < 12*60
  Azimuth2 <- 
    (-180 + abs(Azimuth1))*tmp +    (Azimuth2 <- -180 + abs(Azimuth1))*(!tmp)
  
  #   if (AST < 12*60){
  #     Azimuth2 <- -180 + abs(Azimuth1)
  #   }else{
  #     Azimuth2 <- 180 - Azimuth1
  #   }
  
  #   if (cos(pi/180*HourAngle)> tan(pi/180*Delta)/tan(pi/180*Lat)){
  #     Azimuth  <-  Azimuth1
  #   }else{
  #     Azimuth  <-  Azimuth2
  #   }
  
  tmp <- (cos(pi/180*HourAngle)> tan(pi/180*Delta)/tan(pi/180*Lat))
  Azimuth <- tmp*Azimuth1+Azimuth2*(!tmp)
  #(*Solar Incidence Angle*)
  #(*The angle between sun's ray and the normal on a surface*)
  Incidence  <-  180/pi*acos(sin(pi/180*Lat)*sin(pi/180*Delta)*cos(pi/180*Slope)- cos(pi/180*Lat)*sin(pi/180*Delta)* sin(pi/180*Slope)* cos(pi/180*Aspect)+ cos(pi/180*Lat)*cos(pi/180*Delta)* cos(pi/180*HourAngle)*cos(pi/180*Slope)+         sin(pi/180*Lat)*cos(pi/180*Delta)* cos(pi/180*HourAngle)*sin(pi/180*Slope)*cos(pi/180*Aspect)+        cos(pi/180*Delta)*sin(pi/180*HourAngle)*sin(pi/180*Slope)*sin(pi/180*Aspect)        )
  
  
  a0 <- 0.4237-0.00821*(6-Elevation/1000.0)^2
  a1 <- 0.5055+0.00595*(6.5-Elevation/1000.0)^2
  k <- 0.2711+0.01858*(2.5-Elevation/1000.0)^2
  tb <- (a0+a1*exp(-k/sin(pi/180*Altitude)))*(Altitude>0)
  td <- 0.271-0.294*tb
  
  
  
  Sc <- 1366.1
  Sextr <- Sc*(1+0.033*cos(pi/180*360*DOY/365))
  Sopen <- tb*Sextr
  SextrNormal <- Sextr*cos(pi/180*Incidence)
  
  
  Sdiropen <- Sopen*cos(pi/180*Incidence)
  Sdifopen <- Sopen*td*(sin(pi/180*Altitude))^2*(cos(pi/180*Slope/2.))^2
  
  
  
  list(Daylength=DayLength ,Altitude=Altitude, Azimuth=Azimuth, Incidence=Incidence, Sdiropen=Sdiropen, Sdifopen=Sdifopen)
}
