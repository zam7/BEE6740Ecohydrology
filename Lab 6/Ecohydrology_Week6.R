# Ecohydrology 2018
# In Class Assignment 6

# Load "EcoHydRology" Package
library(EcoHydRology)
library(lubridate)
library(gdata)
library(ggplot2)
setwd("~/Documents/Ecohydrology")

#Step 1: Read in daily temperature data, convert to SI, add a date field
MetData <- read.csv("GameFarmRd_1950-present.csv")
MetData$Precip_mm = MetData$Precip*25.4
MetData$Tmax_C = 5/9*(MetData$Tmax-32)
MetData$Tmin_C = 5/9*(MetData$Tmin-32)
MetData$Date = as.Date(ISOdate(MetData$Year, MetData$Month, MetData$Day))

#Step 2: Calculate average daily temperature for each record
MetData$Tavg_C = (MetData$Tmax_C + MetData$Tmin_C)/2

#Step 3: Run the Walter (2004) Snowmelt model

# Forcing Data - drives entire model to give output 
#Date - Vector of dates (class Date or character) in this format: Y-m-d 
#precip_mm - Vector of precipitation in mm 
#Tmax_C - Vector of daily maximum temperature (degrees C) 
#Tmin_C - Vector of daily minimum temperature (degrees C) 

# Constants - known values, doesn't really contain uncertainty
#lat_deg - Degrees latitude 
lat_deg_Ith = 42.44 #decimal degrees
lat_rad_Ith<-lat_deg_Ith*pi/180 ## latitude in radians
#slope - Overall slope of area of interest 
#aspect - Aspect of the area of interest (compass direction slope faces)
#tempHt - height of temperature measurements (m) 
#windHt - height of wind measurements (m) 

# Model Parameters / Dimensions
#groundAlbedo - Ground Albedo, 0-1 (-) 
#SurfEmissiv - Surface Emissivity, 0-1 (-) 
#windSp - Wind speed - either a vector of measured values or a single value of average wind speed for the site (m/s) 
#forest - Forest cover (shade) - use this only when determining snowmelt under a canopy, 0-1 (-) 

# Initial Condition - can be important for short timeseries, but are unimportant for long timeseries 
#startingSnowDepth_m - The depth of the snow pack initially (m) 

# Run snowmelt model for hydrologic dataset
# function call is SnowMelt
SnowMelt = SnowMelt(MetData$Date, MetData$Precip_mm, MetData$Tmax_C, MetData$Tmin_C, lat_rad_Ith)

#Step 3: Plot a few years of precip, snowfall, and accumulated SWE
plot(SnowMelt$Date[0:(365*3)], SnowMelt$Precip_mm[0:(365*3)], xlab = "Date (years)", ylab = "Water Equivalent (mm)")
lines(SnowMelt$Date[0:(365*3)], SnowMelt$SnowfallWatEq_mm[0:(365*3)], col = "red")
lines(SnowMelt$Date[0:(365*3)], SnowMelt$SnowWaterEq_mm[0:(365*3)], col = "blue")


#ggplot(SnowMelt, aes(x = SnowMelt$Date[0:(365*3)]) + 
 # geom_line(aes(y = SnowMelt$SnowfallWatEq_mm[0:(365*3)])) + 
 # geom_line(aes(y = SnowMelt$SnowWatEq_mm[0:(365*3)])))

#Step 4: How important are the different model parameters ground albedo, windSp, and forest?
#Use Monte Carlo sampling to determine if there is sensitivity to each parameter individually, and if there are higher order interactions among parameters
#Hint: run only two years of the snowmelt model, we're going to be using a lot of CPU power here and want to cut down on run time
#Try running just 10 simulations and see if the results make any sense then try running 2000 simulations

# Define the parameter range
# Albedo
amax = 0.6
amin = 0.05
# Wind speed
umax = 5 #m/s
umin = 0 #m/s
# Forest cover
fcmax = 1
fcmin = 0

n_runs = 1000
# Create random distribution of uniformly distributed of parameters
a_rand = runif(n_runs, min = amin, max = amax)
u_rand = runif(n_runs, min = umin, max = umax)
fc_rand = runif(n_runs, min = fcmin, max = fcmax)

SnowMelt_Results = data.frame(matrix(nrow = n_runs, ncol = 0))

for (i in 1:n_runs)
  {
  snow = SnowMelt(MetData$Date[1:(365*2)], MetData$Precip_mm[1:(365*2)], MetData$Tmax_C[1:(365*2)], MetData$Tmin_C[1:(365*2)], 
                      lat_rad_Ith, groundAlbedo = a_rand[i], windSp = u_rand[i], forest = fc_rand[i])
  SnowMelt_Results$SnowMelt_mm[i] = max(snow$SnowMelt_mm)
  SnowMelt_Results$SWE_mm[i] = max(snow$SnowWaterEq_mm)
  SnowMelt_Results$albedo[i] = a_rand[i]
  SnowMelt_Results$windSp_mps[i] = u_rand[i]
  SnowMelt_Results$forest_cover[i] = fc_rand[i]
  }
  
#Step 5: Plot all first-order parameter sensitivity
par(mfrow=c(2,3))
plot(SnowMelt_Results$albedo, SnowMelt_Results$SnowMelt_mm, xlab = "Albedo", ylab = "Snow Melt (mm)")
plot(SnowMelt_Results$windSp_mps, SnowMelt_Results$SnowMelt_mm, xlab = "U (m/s)", ylab = "Snow Melt (mm)")
plot(SnowMelt_Results$forest_cover, SnowMelt_Results$SnowMelt_mm, xlab = "Forest Cover", ylab = "Snow Melt (mm)")

plot(SnowMelt_Results$albedo, SnowMelt_Results$SWE_mm, xlab = "Albedo", ylab = "SWE (mm)")
plot(SnowMelt_Results$windSp_mps, SnowMelt_Results$SWE_mm, xlab = "U (m/s)", ylab = "SWE (mm)")
plot(SnowMelt_Results$forest_cover, SnowMelt_Results$SWE_mm, xlab = "Forest Cover", ylab = "SWE (mm)")

#Which parameters are sensitive? 
#What does this mean for data collection and experimental design?
#Why is there a lot more scatter than in the one parameter sensitivity?
#What does sensitivity look like? Is there a threshold behavior for any parameters?
#Does parameter sensitivity depend on the question we're asking?

#Step 6: Plot parameter interactions between forest and windspeed for each output
par(mfrow=c(1,2))
maxColorValue <- 100
palette = colorRampPalette(c("blue","red"))(maxColorValue)
plot(SnowMelt_Results$forest_cover, SnowMelt_Results$windSp_mps, 
     col = palette[cut(SnowMelt_Results$SnowMelt_mm, maxColorValue)], 
     xlab = "Forest Cover", ylab = "Wind Speed (m/s)", main = "Snow Melt (mm), red = high snow melt")
plot(SnowMelt_Results$forest_cover, SnowMelt_Results$windSp_mps, 
     col = palette[cut(SnowMelt_Results$SWE_mm, maxColorValue)], 
     xlab = "Forest Cover", ylab = "Wind Speed (m/s)", main = "SWE (mm), red = high SWE")


#How are the parameters related numerically?
#Physically, why would these parameters be related? How does each affect the energy budget?
#What might we have missed if we used a one parameter at a time method of sensitivity?
