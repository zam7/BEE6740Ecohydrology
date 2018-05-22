

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

#Step 1: Import Met Data for Fall Creek
MetData <- read.csv("test_met_final_copy.csv")
MetData$Precip_mm = MetData$Precip*25.4
MetData$Tmax_C = 5/9*(MetData$Tmax-32)
MetData$Tmin_C = 5/9*(MetData$Tmin-32)
MetData$Date = as.Date(ISOdate(MetData$Year, MetData$Month, MetData$Day))
MetData$Tavg_C = (MetData$Tmax_C + MetData$Tmin_C)/2

latitudeDegrees = 41.0965 #decimal degrees
latitudeRadians<-latitudeDegrees*pi/180 ## latitude in radians

MetData <- MetData[which(MetData$Year > 1989 & MetData$Year < 1994),]
FC <- get_usgs_gage(flowgage_id = "04234000", begin_date = "1990-01-01", end_date="1993-12-31")
FC$flowdata$flow  = FC$flowdata$flow / (FC$area*1000)

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
    snowmelt=SnowMelt(Date=MetData$Date, precip_mm=MetData$Precip_mm, Tmax_C=MetData$Tmax_C, Tmin_C=MetData$Tmin_C, lat_deg=latitudeDegrees, 
                      groundAlbedo=0.3, forest=x_test[1])
    
    ModelPrecip = snowmelt$SnowMelt_mm+snowmelt$Rain_mm
    
    Results <- Lumped_VSA_model(dateSeries = MetData$Date, P = ModelPrecip, Tmax=MetData$Tmax_C, Tmin = MetData$Tmin_C, latitudeDegrees=latitudeDegrees, 
                                Depth = 1500, SATper = 0.5, AWCper = 0.17, Tp = x_test[2], StartCond = "avg", BF1 = 1, 
                                albedo=0.3, PETcap = x_test[3], rec_coef = x_test[4], Se_min = x_test[5], C1 = x_test[6], Ia_coef = x_test[7])

    Results$mdate = Results$Date
    AllData = merge(Results, FC$flowdata, by="mdate")
    
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

  plot(AllData$Date, AllData$flow, col = "red", xlab = "Date", ylab = "Discharge (mm)", pch = 20, main = "Post-Construction")
  lines(AllData$Date, AllData$modeled_flow, pch = 21)
  legend( x= "topright", 
          legend=c("Observed Flow","Modeled Flow"), 
          col=c("red","black"),
          pch=c(20, 21), cex = 0.8)
  
#What were the final parameter values?
# forest (%), Tp (hours), PETcap (mm/day), Rec Coef (mm/day), Se min (mm), C1, Ia

#Compare the "best" parameter set for each of the four datasets above. How transferable are the parameters?