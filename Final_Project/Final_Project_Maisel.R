# Ecohydrology 2018
# Final Project
# Analysis of impact of Palisades Mall Construction on Hackensack River at West Nyack, NY

setwd("~/github/Ecohydrology_Modeling")

# Load packages
library(EcoHydRology)
library(lubridate)

# Load USGS gage data for Hackensack River at West Nyack, NY
# Streamgage ID for West Nyack, NY = 01376800

HSR = get_usgs_gage(flowgage_id = "01376800")

#Step 1: Read in daily temperature data, convert to SI, add a date field
# Data collected from NCDC NOAA data for station USC00309270. 
# https://www.ncdc.noaa.gov/cdo-web/datasets/GSOM/locations/ZIP:10994/detail
MetData <- read.csv("test_met_final_copy.csv")
MetData[is.na(MetData)] <- 0
MetData$Precip_mm = MetData$Precip*25.4
MetData$Tmax_C = 5/9*(MetData$Tmax-32)
MetData$Tmin_C = 5/9*(MetData$Tmin-32)
MetData$Date = as.Date(ISOdate(MetData$Year, MetData$Month, MetData$Day))

#Step 2: Calculate average daily temperature for each record
MetData$Tavg_C = (MetData$Tmax_C + MetData$Tmin_C)/2

#Step 3: Run snowmelt model with default parameters
lat_deg_WN = 41.0965 #decimal degrees
lat_rad_WN<-lat_deg_WN*pi/180 ## latitude in radians

###### Analyze period before construction of mall (1990-1992) ### NEED TO EDIT THE INPUTS TO LUMPED_VSA_MODEL
SnowMelt_pre = SnowMelt(MetData$Date[1:(365*3)], MetData$Precip_mm[1:(365*3)], MetData$Tmax_C[1:(365*3)], MetData$Tmin_C[1:(365*3)], lat_rad_WN)

#Step 4: Hydrologic watershed model input = precipitation as rain (mm) + snowmelt (mm)
SnowMelt_pre$Precip_eff_mm = SnowMelt_pre$Rain_mm + SnowMelt_pre$SnowMelt_mm # this takes the precip as rain and the snowmelt to find the 
# effective precipitation as water on land

#Step5: Run Lumped VSA model
#?Lumped_VSA_Mmodel 
LVSAM_pre <- Lumped_VSA_model(dateSeries = SnowMelt_pre$Date, 	P = SnowMelt_pre$Precip_eff_mm, 
                                     Tmax = SnowMelt_pre$MaxT_C, Tmin = SnowMelt_pre$MinT_C, latitudeDegrees = lat_deg_WN, 
                                     Tp = 5, Depth = 2010, SATper = 0.27, AWCper = 0.13, StartCond = "wet")

# Tp: time to peak is how quickly water is turned to runoff
# Will run a sensitivity model on: albedo, PETcap, rec_coef, Se_min, C1, Ia_coef

#Step 6: Plot 5 years of Soil Water, Groundwater Storage (Se), and Discharge
par(mfrow=c(5,1))
par(mar=c(0.5,0.5,0.5,0.5))
plot(LVSAM_pre$Date, SnowMelt_pre$Precip_eff_mm, type = "l", ylab = "Precipitation (mm)")
plot(LVSAM_pre$Date, SnowMelt_pre$SnowWaterEq_mm, type = "l", ylab = "SWE (mm)")
plot(LVSAM_pre$Date, LVSAM_pre$SoilWater, type = "l", ylab = "Soil Moisture, AET")
plot(LVSAM_pre$Date, LVSAM_pre$Se, type = "l", ylab = "Groundwater Storage (Se)")
plot(LVSAM_pre$Date, LVSAM_pre$totQ, type = "l", ylab = "Streamflow")

#Discussion questions
# - We are chaining together different models with different assumptions and therefore error, do water balance errors tend to grow without bounds? Why or why not?

# - We started off with a poor assumption about the catchment water storage, why doesn't this seem to matter? 

# - ET is a function of soil water and PET, but PETc (week 5) was a function of plant growth stage, which should also be a function of soil moisture.
#       We're missing an obvious feedback between soil water and plant growth, but we're not modeling this. How does this limit our predictions?
#       What else does our simple model neglect about plant growth?

#Step 7: First order sensitivity on model "calibration" parameters, choose a metric related to whatever you want

# Step 7a: Look up the EcohydRology package ?Lumped_VSA_Mmodel and read about the parameter meanings
# Decide which parameters of the snowmelt model and lumped_vsa_model are best described as calibration parameters

# Define the parameter range
# Initial abstraction (Ia) - hypothesized as sensitive
Iamax = 0.2
Iamin = 0.05
# Forest cover (Fc) - hypothesized as sensitive
fcmax = 1
fcmin = 0
# Storage (Se) - hypothesized as sensitive
Semax = 150
Semin = 50
# Percent Impervious (PI) - hypothesized as sensitive
PImax = 50
PImin = 0
# Wind speed (u) - hypothesized as unsensitive
umax = 5 #m/s
umin = 0 #m/s

# Step 7b: Choose reasonable ranges for parameter values, perform a Monte Carlo sensitivity
# Choose one metric:
# - ratio of ET to Q
# - peak annual streamflow (flooding) # CHOSEN
# - peak annual overland flow (water quality / runoff)
# - number of days stream discharge < 5 mm/day (drinking water drought)
# - number of days soil water below 180 mm (agricultural drought)

n_runs = 100
# Create random distribution of uniformly distributed of parameters
Ia_rand = runif(n_runs, min = Iamin, max = Iamax)
u_rand = runif(n_runs, min = umin, max = umax)
fc_rand = runif(n_runs, min = fcmin, max = fcmax)
Se_rand = runif(n_runs, min = Semin, max = Semax)
PI_rand = runif(n_runs, min = PImin, max = PImax)

Results_pre = data.frame(matrix(nrow = n_runs, ncol = 0))

for (i in 1:n_runs)
{
  snow = SnowMelt(MetData$Date[1:(365*2)], MetData$Precip_mm[1:(365*2)], MetData$Tmax_C[1:(365*2)], 
                  MetData$Tmin_C[1:(365*2)], lat_rad_WN, windSp = u_rand[i], forest = fc_rand[i])
  
  Precip_eff_mm = snow$Rain_mm + snow$SnowMelt_mm
  
  Lumped_VSA = Lumped_VSA_model(dateSeries = snow$Date, P = Precip_eff_mm, 
                                Tmax = snow$MaxT_C, Tmin = snow$MinT_C, latitudeDegrees = lat_deg_WN, 
                                Tp = 5, Depth = 2010, SATper = 0.27, AWCper = 0.13, StartCond = "wet", 
                                Se_min = Se_rand[i], Ia_coef = Ia_rand[i], percentImpervious = PI_rand[i])
  
  # store the randomized variables
  Results_pre$windSp_mps[i] = u_rand[i]
  Results_pre$forest_cover[i] = fc_rand[i]
  Results_pre$Ia[i] = Ia_rand[i]
  Results_pre$storage_mm[i] = Se_rand[i]
  Results_pre$PI_percent[i] = PI_rand[i]
  
  # store desired outputs
  Results_pre$modeled_flow_mm[i] = max(Lumped_VSA$modeled_flow)
}

par(mfrow=c(2,3))
par(mar=c(2.5,2.5,2.5,2.5))
plot(Results_pre$windSp_mps, Results_pre$modeled_flow_mm, xlab = "Wind speed (m/s)", ylab = "Runoff (mm)")
plot(Results_pre$forest_cover, Results_pre$modeled_flow_mm, xlab = "Forest cover", ylab = "Runoff (mm)")
plot(Results_pre$Ia, Results_pre$modeled_flow_mm, xlab = "Initial abstraction", ylab = "Runoff (mm)")
plot(Results_pre$storage_mm, Results_pre$modeled_flow_mm, xlab = "Storage", ylab = "Runoff (mm)")
plot(Results_pre$PI_percent, Results_pre$modeled_flow_mm, xlab = "Percent Impervious", ylab = "Runoff (mm)")

#Step 8: Compute Nash Sutcliffe Model Efficiency

# Get data for Fall Creek to use as "observed" values
NS_HSR_obs = get_usgs_gage(flowgage_id = "01376800", begin_date = "1990-01-01", end_date="1992-12-30")
NS_HSR_obs$flowrate_mmperd = NS_HSR_obs$flowdata$flow / (NS_HSR_obs$area*1000) # flow is given in cubic meters per day, so we convert to mm/day

# Use VSA model with default inputs for "simulated" values
NS_HSR_sim <- LVSAM_pre

# Create an empty matrix to be populated with values for NSE equation
AllData = data.frame(matrix(nrow = (365*3), ncol = 0))

AllData$sim_obs = NS_HSR_sim$modeled_flow - NS_HSR_obs$flowrate_mmperd
NSE_numerator = sum(AllData$sim_obs * AllData$sim_obs)
AllData$obs_obs = NS_HSR_obs$flowrate_mmperd - mean(NS_HSR_obs$flowrate_mmperd)
NSE_denom = sum(AllData$obs_obs * AllData$obs_obs)

NSE = 1 - (NSE_numerator/NSE_denom)
# result is NSE = -1.73

################################################################################################################################################################################################################
################################################################################################################################################################################################################
################################################################################################################################################################################################################
# Run calculations to determine the best parameter as inputs to the models

# Returns numIter length list of entries to be peturbed
probPeturb<-function(x, numIter){
  # Input is xBounds & numIter.  
  # Returns numIter entry list with the indices which will be peturbed
  xDims<-nrow(x)
  probabilityVector<-1-log(1:numIter)/log(numIter)
  peturbIdx<-apply(matrix(unlist(lapply(probabilityVector, function(x) as.logical(rbinom(xDims, 1, x)))), byrow=TRUE, ncol=xDims), 1, which)
  return(peturbIdx)
}

MetData <- MetData[which(MetData$Year > 1989 & MetData$Year < 1994),]
#NS_HSR_obs$flowrate_mmperd

#Skip Down to Step 3

#This is the DDS Algorithm, I've coded it in full here to save us some time, Skip down to Step 4
# Define Calibration Parameters
xBounds.df = data.frame(matrix(ncol=2,nrow=7))
colnames(xBounds.df)<-c("min", "max")

#forest
xBounds.df$min[1] = 0.1
xBounds.df$max[1] = 0.9

#Tp
xBounds.df$min[2] = 1
xBounds.df$max[2] = 10

#PETcap
xBounds.df$min[3] = 4
xBounds.df$max[3] = 6

#rec_coef
xBounds.df$min[4] = 0.01
xBounds.df$max[4] = 0.2

#Se_min
xBounds.df$min[5] = 50
xBounds.df$max[5] = 150

#C1
xBounds.df$min[6] = 1
xBounds.df$max[6] = 5

#Ia_coef
xBounds.df$min[7] = 0.05
xBounds.df$max[7] = 0.2

# Generate initial first guess
#xBounds.df<-data.frame(col1 = rep(10,10), col2=rep(100, 10))
x_init<-c(0.1, 1, 4, 0.01, 50, 1, 0.05)
x_best = data.frame(x_init)

# Evaluate first cost function
NSE_init = -9999
NSE_best<-NSE_init

r= 0.2
numIter = 100
# Select which entry to peturb at each iteration

peturbIdx<-probPeturb(xBounds.df, numIter)
# Peturb each entry by N(0,1)*r(x_max - x_min) reflecting if @ boundaries
sigma<-xBounds.df$max - xBounds.df$min


for (i in 2:numIter){
  
  # Set up test x
  x_test<-as.matrix(x_best)
  
  # Get entries we will peturb
  idx<-peturbIdx[[i]]
  
  # Initialize vector of peturbations initially zeros with same length of x so we will add this vector to peturb x
  peturbVec<-rep(0, length(x_test))
  # Generate the required number of random normal variables
  N<-rnorm(length(x_test), mean=0, sd=1)
  
  # Set up vector of peturbations
  peturbVec[idx]<-r*N[idx]*sigma[idx]
  
  # Temporary resulting x value if we peturbed it
  testPeturb<-x_test + peturbVec  
  # Find the values in testPeturb that have boundary violations.  Store the indices in boundaryViolationsIdx
  boundaryViolationIdx<-which(testPeturb<xBounds.df$min | testPeturb > xBounds.df$max)
  
  # Reset those violated indices to the opposite peturbation direction
  peturbVec[boundaryViolationIdx]<-(-1*r*N[boundaryViolationIdx]*sigma[boundaryViolationIdx])
  
  # Find values still at violations of min or max and set them to the minimum or maximum values
  testPeturb<-x_test + peturbVec
  minViolationIdx<-which(testPeturb<xBounds.df$min)
  maxViolationIdx<-which(testPeturb>xBounds.df$max)
  testPeturb[minViolationIdx]<-xBounds.df$min[minViolationIdx]
  testPeturb[maxViolationIdx]<-xBounds.df$max[maxViolationIdx]
  
  # Peturb the test vector
  x_test<-x_test + peturbVec  
  
  # Run the Lumped_VSA_Model
  snowmelt=SnowMelt(Date=MetData$Date, precip_mm=MetData$Precip, Tmax_C=MetData$Tmax_C, Tmin_C=MetData$Tmin_C, lat_deg=lat_deg_WN, 
                    groundAlbedo=0.3, forest=x_test[1])
  
  ModelPrecip = snowmelt$SnowMelt_mm+snowmelt$Rain_mm
  
  Results <- Lumped_VSA_model(dateSeries = MetData$Date, P = ModelPrecip, Tmax=MetData$Tmax_C, Tmin = MetData$Tmin_C, latitudeDegrees=lat_deg_WN, 
                              Depth = 1500, SATper = 0.5, AWCper = 0.17, Tp = x_test[2], StartCond = "avg", BF1 = 1, 
                              albedo=0.3, PETcap = x_test[3], rec_coef = x_test[4], Se_min = x_test[5], C1 = x_test[6], Ia_coef = x_test[7])
  
  Results$mdate = Results$Date
  AllData = merge(Results, NS_HSR_obs$flowrate_mmperd, by="mdate")
  
  #Step 3: we're running a "spin-up" year to set initial conditions, but we only want to calculate NSE on our study year     
  #Calculate NSE
  
  numer = 0
  denom = 0
  for (j in 1:nrow(AllData))
  {numer = numer + (AllData$modeled_flow[j] - AllData$flow[j])^2
  denom = denom + (AllData$flow[j] - mean(AllData$flow))^2}
  NSE = 1 - numer/denom
  
  #Check if this simulation is better
  if (NSE > NSE_best)
  {
    x_best = x_test
    NSE_best = NSE      
  }
  print_str = paste("Eval:",i,"   NSE:",NSE,"   NSE Best:",NSE_best)
  print(print_str)
  
}

#Step 4: Plot our "globally best" model simulated discharge vs. observed data

plot(AllData$Date, AllData$flow, col = "red", xlab = "Date", ylab = "Discharge (mm)", pch = 20)
lines(AllData$Date, AllData$modeled_flow, pch = 21)
legend( x= "topright", 
        legend=c("Observed Flow","Modeled Flow"), 
        col=c("red","black"),
        pch=c(20, 21), cex = 0.5)

#What were the final parameter values?
# forest (%), Tp (hours), PETcap (mm/day), Rec Coef (mm/day), Se min (mm), C1, Ia

#Compare the "best" parameter set for each of the four datasets above. How transferable are the parameters?
