# Zoe Maisel
# Ecohydrology 2017
# In Class Assignment 4

# Load "EcoHydRology" Package
library(EcoHydRology)
library(lubridate)
setwd("~/Documents/Ecohydrology")
latitudeDegrees_Ith = 42.44 #decimal degrees
latitudeRadians_Ith<-latitudeDegrees_Ith*pi/180 ## latitude in radians

# Step 2: convert to SI units
MetData_Ith <- read.csv("GameFarmRd_1950-present.csv")
for (i in 1:nrow(MetData_Ith))
{
  MetData_Ith$Precip_mm[i] = MetData_Ith$Precip[i]*25.4
  MetData_Ith$Tmax_C[i] = 5/9*(MetData_Ith$Tmax[i]-32)
  MetData_Ith$Tmin_C[i] = 5/9*(MetData_Ith$Tmin[i]-32)
  MetData_Ith$Tavg_C[i] = (MetData_Ith$Tmax_C[i]+MetData_Ith$Tmin_C[i])/2
}  

# Step 3: Add a julian date column
MetData_Ith$Date = as.Date(ISOdate(MetData_Ith$Year, MetData_Ith$Month, MetData_Ith$Day))
x=as.POSIXlt(MetData_Ith$Date, format="Y%b%d")
MetData_Ith$JulianDate = x$yday + 1

# Step 4: calculate Pet in mm from Ecohydrology package using "PET_fromTemp" function
MetData_Ith$PET_mm = PET_fromTemp(MetData_Ith$JulianDate, MetData_Ith$Tmax_C, MetData_Ith$Tmin_C, latitudeRadians_Ith, AvgT = (MetData_Ith$Tmax_C + MetData_Ith$Tmin_C)/2, albedo = 0.3)*1000 # the output units from the function was in meters so we needed to multiply by 1000 to get to mm

# Step 5: Plot the first five years of daily PET estimates and air temperatures
#describe the seasonality, does this make sense?
par(mfrow=c(1,1))
par(mar = c(5, 5, 5, 5))
plot(MetData_Ith$Date[0:(365*5)],MetData_Ith$PET_mm[0:(365*5)], xlab = "Date (years)", ylab = "PET Ithaca (mm)")

# Step 6: Plot PET against daily average temperature by month
#These lines create an array of empty plots, now just use plot() to fill it in
par(mfrow=c(3,4))
par(mar=c(5,5,3,1.5))

Monthly_FC_Data = data.frame(matrix(nrow = length(MetData_Ith$Month), ncol = 0))
for (month in 1:12)
{
  Monthly_FC_Data = MetData_Ith[MetData_Ith$Month == month,] # populates Monthly_FC_Data with months
  plot(Monthly_FC_Data$Tavg_C,Monthly_FC_Data$PET_mm,ylim = c(0,8),xlab = "Temperature (C)",ylab = "PET Ithaca (mm)")
}
# The slope of the graph has to do with the tilt of the earth. The actual spread over a given month - has to do with longwave radiation (if it's cloudy, there is a lot of longwave radiation). 
# Humidity, albedo, long-wave radiation, seasonality. Ex: clouds can emit at night 

#Step 7: Sensitivity of PET to albedo
#Step 8: Graphically plot your PET metric against albedo to estimate first-order sensitivity (d PET / d albedo)

# determine how sensitive PET is to changes in albedo
# rerun the PET model with varied values for albedo and analyze the output to determine the sensitivity of PET to albedo
# It may be helpful to choose a simlpe metric of PET that is ecologically or hydrologically meaningful - choose annual daily maximum
par(mfrow=c(1,1))
par(mar = c(5, 5, 5, 5))
frame()
albedo = 0
while (albedo < 1)
{
  MetData_Ith$PET_mm_alb = PET_fromTemp(MetData_Ith$JulianDate, MetData_Ith$Tmax_C, MetData_Ith$Tmin_C, latitudeRadians_Ith, AvgT = (MetData_Ith$Tmax_C + MetData_Ith$Tmin_C)/2, albedo = albedo)*1000 # the output units from the function was in meters so we needed to multiply by 1000 to get to mm
  plotable = mean(MetData_Ith$PET_mm_alb)
  plot(albedo,plotable,ylim = c(0,4),xlab = "Albedo",ylab = "Avg PET Ithaca (mm/day)", xlim = c(0,1))
  par(new=T)
  albedo = albedo + 0.05
}

# This shows that there is sensitivity below an albedo of 0.8 and it is not sensitive above 0.8 because the slope of the line flattens out. 

##################################################################################################################################################
#Step 9: Calcuate PET for the alternate site you used in week 3, describe how they compare.
# Obtained precipitation and temperature data from NOAA "Global Summary of the Year" for the Albuquerque Airport Station 

latitudeDegrees_Alb = 35.09 #decimal degrees
latitudeRadians_Alb<-latitudeDegrees_Alb*pi/180 ## latitude in radians

MetData_Alb <- read.csv("AlbuquerquPrecipTempData.csv")
for (i in 1:nrow(MetData_Alb))
{
  MetData_Alb$Precip_mm[i] = MetData_Alb$PRCP[i]*25.4
  MetData_Alb$Tmax_C[i] = 5/9*(MetData_Alb$TMAX[i]-32)
  MetData_Alb$Tmin_C[i] = 5/9*(MetData_Alb$TMIN[i]-32)
  MetData_Alb$Tavg_C[i] = (MetData_Alb$Tmax_C[i]+MetData_Alb$Tmin_C[i])/2
}  

# Step 3: Add a julian date column
MetData_Alb$Date = as.Date(MetData_Alb$DATE) # converts given DATE column into appropriate format
x=as.POSIXlt(MetData_Alb$Date, format="Y%b%d")
MetData_Alb$JulianDate = x$yday + 1

# Step 4: calculate Pet in mm from Ecohydrology package using "PET_fromTemp" function
MetData_Alb$PET_mm = PET_fromTemp(MetData_Alb$JulianDate, MetData_Alb$Tmax_C, MetData_Alb$Tmin_C, latitudeRadians_Alb, AvgT = (MetData_Alb$Tmax_C + MetData_Alb$Tmin_C)/2, albedo = 0.3)*1000 # the output units from the function was in meters so we needed to multiply by 1000 to get to mm

# Step 5: Plot the first five years of daily PET estimates and air temperatures
#describe the seasonality, does this make sense?
par(mfrow=c(1,1))
par(mar = c(5, 5, 5, 5))
plot(MetData_Alb$Date[0:(365*5)],MetData_Alb$PET_mm[0:(365*5)], xlab = "Date (years)", ylab = "PET Albuquerque (mm)")

# Step 6: Plot PET against daily average temperature by month
#These lines create an array of empty plots, now just use plot() to fill it in
par(mfrow=c(3,4))
par(mar=c(5,5,3,1.5))

# extracts Month from the Date column for MetData_Alb
MetData_Alb$Month = month(MetData_Alb$Date)
Monthly_Alb_Data = data.frame(matrix(nrow = length(MetData_Alb$Month), ncol = 0))
for (month in 1:12)
{
  Monthly_Alb_Data = MetData_Alb[MetData_Alb$Month == month,] # populates Monthly_Alb_Data with months
  plot(Monthly_Alb_Data$Tavg_C,Monthly_Alb_Data$PET_mm, ylim = c(0,8),xlab = "Temperature (C)",ylab = "PET Albuquerque (mm)")
}

# The slope of the graph has to do with the tilt of the earth. The actual spread over a given month - has to do with longwave radiation (if it's cloudy, there is a lot of longwave radiation). 
# Humidity, albedo, long-wave radiation, seasonality. Ex: clouds can emit at night 

#Step 7: Sensitivity of PET to albedo
#Step 8: Graphically plot your PET metric against albedo to estimate first-order sensitivity (d PET / d albedo)

# determine how sensitive PET is to changes in albedo
# rerun the PET model with varied values for albedo and analyze the output to determine the sensitivity of PET to albedo
# It may be helpful to choose a simlpe metric of PET that is ecologically or hydrologically meaningful - choose annual daily maximum
par(mfrow=c(1,1))
par(mar = c(5, 5, 5, 5))
frame()
albedo = 0
while (albedo < 1)
{
  MetData_Alb$PET_mm_alb = PET_fromTemp(MetData_Alb$JulianDate, MetData_Alb$Tmax_C, MetData_Alb$Tmin_C, latitudeRadians_Alb, AvgT = (MetData_Alb$Tmax_C + MetData_Alb$Tmin_C)/2, albedo = albedo)*1000 # the output units from the function was in meters so we needed to multiply by 1000 to get to mm
  plotable = mean(MetData_Alb$PET_mm_alb)
  plot(albedo,plotable,ylim = c(0,5),xlab = "Albedo",ylab = "Avg PET Albuquerque (mm/day)", xlim = c(0,1))
  par(new=T)
  albedo = albedo + 0.05
}

#####################################################################################################################
# Comparisons 

# Albedo comparison
# Black is Ithaca, Red is Albuquerque
par(mfrow=c(1,1))
par(mar = c(5, 5, 5, 5))
frame()
albedo = 0
while (albedo < 1)
{
  MetData_Ith$PET_mm_alb = PET_fromTemp(MetData_Ith$JulianDate, MetData_Ith$Tmax_C, MetData_Ith$Tmin_C, latitudeRadians_Ith, AvgT = (MetData_Ith$Tmax_C + MetData_Ith$Tmin_C)/2, albedo = albedo)*1000 # the output units from the function was in meters so we needed to multiply by 1000 to get to mm
  MetData_Alb$PET_mm_alb = PET_fromTemp(MetData_Alb$JulianDate, MetData_Alb$Tmax_C, MetData_Alb$Tmin_C, latitudeRadians_Alb, AvgT = (MetData_Alb$Tmax_C + MetData_Alb$Tmin_C)/2, albedo = albedo)*1000 # the output units from the function was in meters so we needed to multiply by 1000 to get to mm
  plotable_Ith = mean(MetData_Ith$PET_mm_alb)
  plotable_Alb = mean(MetData_Alb$PET_mm_alb)
  plot(albedo,plotable_Ith,ylim = c(0,5),xlab = "Albedo",ylab = "Avg PET (mm/day)", xlim = c(0,1))
  points(albedo,plotable_Alb,ylim = c(0,5), col = "red")
  par(new=T)
  albedo = albedo + 0.05
}
