# Week 8
# Zoe Maisel

setwd("~/github/Ecohydrology_Modeling")

# Returns numIter length list of entries to be peturbed
probPeturb<-function(x, numIter){
  # Input is xBounds & numIter.  
  # Returns numIter entry list with the indices which will be peturbed
  xDims<-nrow(x)
  probabilityVector<-1-log(1:numIter)/log(numIter)
  peturbIdx<-apply(matrix(unlist(lapply(probabilityVector, function(x) as.logical(rbinom(xDims, 1, x)))), byrow=TRUE, ncol=xDims), 1, which)
  return(peturbIdx)
}


# Load "EcoHydRology" Package
library(EcoHydRology)
library(lubridate)

#Step 1: Choose a USGS gage, go to NCDC data online and find daily precipitation, daily minimum temperature, and daily maximum temperature for a watershed
#Make sure you have at least 10 continuous years of data
#Check the NCDC met data for gaps, Precip = flag, TMAX < TMIN, etc.
#You can use Fall Creek to get this working, but then for your assignment pick a different watershed

MetData <- read.csv("AlbuquerquePrecipTempData.csv")
MetData$Precip_mm = MetData$PRCP*25.4
MetData$Tmax_C = 5/9*(MetData$TMAX-32)
MetData$Tmin_C = 5/9*(MetData$TMIN-32)
y = MetData$DATE
MetData$Date_mdy = mdy(y)
MetData$Year = year(MetData$Date_mdy)

# make sure that the years are in the correct century
for (i in 1:nrow(MetData))
{
   if (MetData$Year[i] > 2016)
  {
    MetData$Year[i] = MetData$Year[i] - 100
   }
}

MetData$Tavg_C = (MetData$Tmax_C + MetData$Tmin_C)/2

latitudeDegrees = 35.0844 #decimal degrees
latitudeRadians<-latitudeDegrees*pi/180 ## latitude in radians

#Step 2: Remove all data except the one recent decade
MetData <- MetData[which(MetData$Year > 1999 & MetData$Year < 2010),]

# Download USGS streamflow data for Rio Grande, Albuquerque, NM 
# https://waterdata.usgs.gov/nwis/uv?site_no=08330000
# Albuquerque, New Mexico: "08330000"
RG <- get_usgs_gage(flowgage_id = "08330000", begin_date = "2000-01-01", end_date="2009-12-31")
RG$flowdata$flow  = RG$flowdata$flow / (RG$area*1000)

#This is the DDS Algorithm, I've coded it in full here to save us some time, Please read through it down to Step 4
  
  # Define Calibration Parameters and Feasible Parameter Ranges
  xBounds.df = data.frame(matrix(ncol=2,nrow=8))
  colnames(xBounds.df)<-c("min", "max")

  #forest
  xBounds.df$min[1] = 0.1
  xBounds.df$max[1] = 0.9
  
  #Tp
  xBounds.df$min[2] = 1
  xBounds.df$max[2] = 100 # this is a large watershed so it takes longer for water to reach the stream to peak

  #PETcap
  xBounds.df$min[3] = 4
  xBounds.df$max[3] = 6
  
  #rec_coef, bigger watershed cannot respond as quickly so everything decreases
  xBounds.df$min[4] = 0.001
  xBounds.df$max[4] = 0.1
    
  #Se_min
  xBounds.df$min[5] = 50
  xBounds.df$max[5] = 150
    
  #C1
  xBounds.df$min[6] = 1
  xBounds.df$max[6] = 5
    
  #Ia_coef
  xBounds.df$min[7] = 0.05
  xBounds.df$max[7] = 0.2
  
  #Perct_Imp
  xBounds.df$min[8] = 0
  xBounds.df$max[8] = 0.5
  
  
  # Generate initial first guess
  #xBounds.df<-data.frame(col1 = rep(10,10), col2=rep(100, 10))
  x_init<-c(0.1, 1, 4, 0.01, 50, 1, 0.05, 0)
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


  for (i in 2:numIter)
    {
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
    snowmelt=SnowMelt(Date=MetData$Date, precip_mm=MetData$Precip_mm, Tmax_C=MetData$Tmax_C, Tmin_C=MetData$Tmin_C, lat_deg=latitudeDegrees, 
                      groundAlbedo=0.3, forest=x_test[1])
    
    ModelPrecip = snowmelt$SnowMelt_mm+snowmelt$Rain_mm
    
    # The AWCper was changed to 0.02 based on the 
    
    Results <- Lumped_VSA_model(dateSeries = MetData$Date, P = ModelPrecip, Tmax=MetData$Tmax_C, Tmin = MetData$Tmin_C, latitudeDegrees=latitudeDegrees, 
                                Depth = 1500, SATper = 0.5, AWCper = .02, Tp = x_test[2], StartCond = "avg", BF1 = 1, 
                                albedo=0.3, PETcap = x_test[3], rec_coef = x_test[4], Se_min = x_test[5], C1 = x_test[6], Ia_coef = x_test[7], percentImpervious = x_test[8])

    Results$mdate = Results$Date
    #Step X: Merge modeled flow with observed flow
    AllData = merge(Results, RG$flowdata, by="mdate")
    
    
    #Calculate NSE
    numer = 0
    denom = 0
    
    #Step 4: Code in the NSE and pass it through to the algorithm in the next if statement
    AllData$sim_obs = AllData$modeled_flow - AllData$flow
    NSE_numerator = sum(AllData$sim_obs * AllData$sim_obs)
    AllData$obs_obs = AllData$flow - mean(AllData$flow)
    NSE_denom = sum(AllData$obs_obs * AllData$obs_obs)
    NSE = 1 - (NSE_numerator/NSE_denom)
    
    # Create a new column 
    
    #Check if this simulation is better
    if (NSE > NSE_best)
    {
      x_best = x_test
      NSE_best = NSE      
    }
    print_str = paste("Eval:",i,"   NSE:",NSE,"   NSE Best:",NSE_best)
    print(print_str)
    
  }
  
#Step 5: Plot our "globally best" model simulated discharge vs. observed data

# Need to calculate the "globally best" model using the parameters determined in x_best
  Snowmelt_Best=SnowMelt(Date=MetData$Date, precip_mm=MetData$Precip_mm, Tmax_C=MetData$Tmax_C, Tmin_C=MetData$Tmin_C, lat_deg=latitudeDegrees, 
                    groundAlbedo=0.3, forest=x_best[1])
  
  ModelPrecip_Best = Snowmelt_Best$SnowMelt_mm + Snowmelt_Best$Rain_mm
  
  # This is the "globally best" model that we want to plot vs. the observed data
  Globally_Best <- Lumped_VSA_model(dateSeries = MetData$Date, P = ModelPrecip_Best, Tmax=MetData$Tmax_C, Tmin = MetData$Tmin_C, latitudeDegrees=latitudeDegrees, 
                              Depth = 1500, SATper = 0.5, AWCper = 0.02, Tp = x_best[2], StartCond = "avg", BF1 = 1, 
                              albedo=0.3, PETcap = x_best[3], rec_coef = x_best[4], Se_min = x_best[5], C1 = x_best[6], Ia_coef = x_best[7], percentImpervious = x_best[8])
  
  Globally_Best$mdate = Globally_Best$Date
  # Merge modeled flow with observed flow
  BestAllData = merge(Globally_Best, RG$flowdata, by="mdate")
  
  plot(BestAllData$Date, BestAllData$flow, main = "Globally Best Model and observed data for Rio Grande flow", ylab = "Flow (mm/day)", xlab = "Date")
  lines(BestAllData$Date,BestAllData$modeled_flow,  col = "red")
  legend( x= "topright", 
          legend=c("Observed Flow","Modeled Flow"), 
          col=c("black","red"),
          pch=c(21, 21), cex = 0.5)
  
#Plot the simulated and observed data on a semilog y axis. What are the implications of this model structure for low stream flows?
#Hint: plot(x,y,....,log="y")

  plot(BestAllData$Date, BestAllData$modeled_flow,  col = "red", log="y", main = "Globally Best Model and observed data for Rio Grande flow", ylab = "Log of Flow (mm/day)", xlab = "Date")
  lines(BestAllData$Date, BestAllData$flow, log="y")
  legend( x= "topright", 
          legend=c("Observed Flow","Modeled Flow"), 
          col=c("black","red"),
          pch=c(21, 21), cex = 0.5)
  
#Step 6: Plot state variables (ET, SWE, Soil Moisture, Streamflow)
par(mfrow=c(6,1))
par(mar=c(2.5,5,1.5,1.5))
plot(BestAllData$Date, snowmelt$Precip_mm,xlab = 'Date',ylab="Precip (mm)",pch=16)
plot(BestAllData$Date, snowmelt$SnowWaterEq_mm, xlab = 'Date', ylab="SWE (mm)",pch=16)
plot(BestAllData$Date, BestAllData$ET,xlab = 'Date',ylab="ET (mm)",pch=16)
plot(BestAllData$Date, BestAllData$SoilWater,xlab = 'Date',ylab="Soil Water (mm)",pch=16)
plot(BestAllData$Date, BestAllData$modeled_flow,xlab = 'Date',ylab="Modeled Discharge (mm)",pch=16)
plot(BestAllData$Date, RG$flowdata$flow, xlab = 'Date', ylab="Observed Discharge (mm)",pch=16)

#Let's compare differences in state variables and model parameters across all of our watersheds

#What inferences can we make about ecohydrology with a model and a calibration algorithm?

# This model does not work for this watershed for a few reasons
# 1. The Rio Grande is a highly controlled river, so the observed flow that we get is not actually the natural flow. 
# Any model that we try to compare to the observed flow will not make sense because the observed flow is not a result of the hydrologic cycles.
# 2. This is a highly urbanized watershed, but we tried to account for that by adding the percent impervious parameter 
# 3. The model was modified to consider the sandy soil and small soil depth for NM (as compared with NY)
