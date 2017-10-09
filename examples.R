library(calcSolar)

#Calculating solar altitude angle for two consecutive days

DOY <- seq(0, 2, .05)

alpha <- Altitude(DOY, Lat = 45, Lon=0, SLon=0, DS=60)
#Note: only the difference between Lon and SLon matters not each value

plot(DOY, alpha)




#Calculating apparent solar time for two consecutive days

DOY <- seq(0, 2, .05)

ast <- AST(DOY, Lon=0, SLon=0, DS=60)
#Note: only the difference between Lon and SLon matters not each value

plot(DOY, ast)




#Calculating solar azimuth angle for two consecutive days on 45 degree latitude and 10 degree longitude

DOY <- seq(0, 2, .05)

Az <- Azimuth(DOY, Lat = 45, Lon=10, SLon=10, DS=0)
#Note: only the difference between Lon and SLon matters not each value

plot(DOY, Az)




#Calculating day length for 365 day of the year for 45 degree latitude

DOY <- 1:365

Lat = 45

dl <- DayLength(DOY, Lat)

plot(DOY, dl)



#Calculating day of year for now

DayOfYear(Sys.time())



#Calculating solar declination angle for 365 day of the year

DOY <- 1:365

delta <- Declination(DOY)

plot(DOY, delta)




#Calculating atmospheric transmittance coefficient for two consecutive days on 45 degree latitude and 10 degree longitude and at 100 m altitude.

DOY <- seq(0, 2, .01)

Sdifopen <- DiffuseRadiation(DOY, Lat = 45, Lon=10, SLon=10, DS=0, Elevation = 100, Slope = 0)
#Note: only the difference between Lon and SLon matters not each value

plot(DOY, Sdifopen)





#Calculating atmospheric diffusion factor for two consecutive days on 45 degree latitude and 10 degree longitude and at 100 m altitude.

DOY <- seq(0, 2, .05)

td <- DiffusionFactor(DOY, Lat = 45, Lon=10, SLon=10, DS=0, Elevation = 100)
#Note: only the difference between Lon and SLon matters not each value

plot(DOY, td)






#Calculating atmospheric transmittance coefficient for two consecutive days on 45 degree latitude and 10 degree longitude and at 100 m altitude.

DOY <- seq(0, 2, .05)

Sopen <- OpenRadiation(DOY, Lat = 45, Lon=10, SLon=10, DS=0, Elevation = 100)
#Note: only the difference between Lon and SLon matters not each value

plot(DOY, Sopen)




#Calculating equaiton of time for 365 day of the year

DOY <- 1:365

eot <- EOT(DOY)

plot(DOY, eot)



#Calculating solar extraterrestrial radiation for 365 day of the year

DOY <- 1:365

Sextr <- Extraterrestrial(DOY)

plot(DOY, Sextr)




#Calculating solar incidence angle for two consecutive days on 45 degree latitude and 10 degree longitude

DOY <- seq(0, 2, .05)

SextrNormal <- ExtraterrestrialNormal(DOY, Lat = 45, Lon=10, SLon=10, DS=0, Slope = 10, Aspect = 0)
#Note: only the difference between Lon and SLon matters not each value

plot(DOY, SextrNormal)





#Calculating solar hour angle for two consecutive days

DOY <- seq(0, 2, .01)

h <- HourAngle(DOY, Lon=0, SLon=0, DS=60)
#Note: only the difference between Lon and SLon matters not each value

plot(DOY, h)




#Calculating solar incidence angle for two consecutive days on 45 degree latitude and 10 degree longitude

DOY <- seq(180, 182, .05)

theta <- Incidence(DOY, Lat = 45, Lon=10, SLon=10, DS=0, Slope = 0, Aspect = 0)
#Note: only the difference between Lon and SLon matters not each value

plot(DOY, theta)



#Calculating  local standard time for two consecutive days

DOY <- seq(0, 2, .05)

lst <- LST(DOY)

plot(DOY, lst)




#Calculating open sky solar radiation for two consecutive days on 45 degree latitude and 10 degree longitude and at 100 m altitude.

DOY <- seq(0, 2, .05)

Sopen <- OpenRadiation(DOY, Lat = 45, Lon=10, SLon=10, DS=0, Elevation = 100)
#Note: only the difference between Lon and SLon matters not each value

plot(DOY, Sopen)



#Calculating solar variables angle for two consecutive days on 45 degree latitude and 10 degree longitude

DOY <- seq(0, 2, .05)

solar <- Solar(DOY, Lat = 45, Lon=10, SLon=10, DS=0, Slope = 10, Aspect = 0)
#Note: only the difference between Lon and SLon matters not each value
par(mfrow=c(3,1))
plot(DOY, solar$Altitude, ylim = c(-90,90))
plot(DOY, solar$Azimuth, col= 'red')

plot(DOY, solar$Sdiropen)
lines(DOY, solar$Sdifopen, col='red')



#Calculating sunrise time for 365 day of the year for 45 degree latitude

DOY <- 1:365

Lat = 45

sunrise <- Sunset(DOY, Lat)

plot(DOY, sunrise)




#Calculating sunset time for 365 day of the year for 45 degree latitude

DOY <- 1:365

Lat = 45

sunset <- Sunset(DOY, Lat)

plot(DOY, sunset)
