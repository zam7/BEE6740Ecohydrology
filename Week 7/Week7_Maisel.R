# Ecohydrology 2018
# In Class Assignment 7

setwd("~/github/BEE6740Ecohydrology")

# Load "EcoHydRology" Package
library(EcoHydRology)

#Step 1: Read in daily temperature data, convert to SI, add a date field
MetData <- read.csv("GameFarmRd_1950-present.csv")
MetData$Precip_mm = MetData$Precip*25.4
MetData$Tmax_C = 5/9*(MetData$Tmax-32)
MetData$Tmin_C = 5/9*(MetData$Tmin-32)
MetData$Date = as.Date(ISOdate(MetData$Year, MetData$Month, MetData$Day))

#Step 2: Calculate average daily temperature for each record
MetData$Tavg_C = (MetData$Tmax_C + MetData$Tmin_C)/2

#Step 3: Run snowmelt model with default parameters
lat_deg_Ith = 42.44 #decimal degrees
lat_rad_Ith<-lat_deg_Ith*pi/180 ## latitude in radians

#Step 4: Hydrologic watershed model input = precipitation as rain (mm) + snowmelt (mm)
SnowMelt = SnowMelt(MetData$Date, MetData$Precip_mm, MetData$Tmax_C, MetData$Tmin_C, lat_rad_Ith)
SnowMelt$Precip_eff_mm = SnowMelt$Rain_mm + SnowMelt$SnowMelt_mm # this takes the precip as rain and the snowmelt to find the 
                                                                # effective precipitation as water on land

#Step5: Run Lumped VSA model
#?Lumped_VSA_Mmodel 
Lumped_VSA_Model <- Lumped_VSA_model(dateSeries = SnowMelt$Date, 	P = SnowMelt$Precip_eff_mm, 
                            Tmax=SnowMelt$MaxT_C, Tmin = SnowMelt$MinT_C, latitudeDegrees = lat_deg_Ith, Tp = 5.8, Depth = 2010, 
                            SATper = 0.27, AWCper = 0.13, StartCond = "wet")

# Tp: time to peak is how quickly water is turned to runoff
# Will run a sensitivity model on: albedo, PETcap, rec_coef, Se_min, C1, Ia_coef

#Step 6: Plot 5 years of Soil Water, Groundwater Storage (Se), and Discharge

#Discussion questions
# - We are chaining together different models with different assumptions and therefore error, do water balance errors tend to grow without bounds? Why or why not?

# - We started off with a poor assumption about the catchment water storage, why doesn't this seem to matter? 

# - ET is a function of soil water and PET, but PETc (week 5) was a function of plant growth stage, which should also be a function of soil moisture.
#       We're missing an obvious feedback between soil water and plant growth, but we're not modeling this. How does this limit our predictions?
#       What else does our simple model neglect about plant growth?

#Step 7: First order sensitivity on model "calibration" parameters, choose a metric related to whatever you want
#
# Step 7a: Look up the EcohydRology package ?Lumped_VSA_Mmodel and read about the parameter meanings
# Decide which parameters of the snowmelt model and lumped_vsa_model are best described as calibration parameters

# Step 7b: Choose reasonable ranges for parameter values, perform a Monte Carlo sensitivity
# Choose one metric:
# - ratio of ET to Q
# - peak annual streamflow (flooding)
# - peak annual overland flow (water quality / runoff)
# - number of days stream discharge < 5 mm/day (drinking water drought)
# - number of days soil water below 180 mm (agricultural drought)

#Step 8: Compute Nash Sutcliffe Model Efficiency