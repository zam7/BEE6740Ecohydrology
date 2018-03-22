# Ecohydrology 2018
# In Class Assignment 9

setwd("~/github/Ecohydrology_Modeling")

# Load "EcoHydRology" Package
library(EcoHydRology)

#Step 1: Read in all 5 climate datasets (already in SI units) and convert date to appropriate format
#NEX-GDDP (https://nex.nasa.gov/nex/projects/1356/)
#Raw GCM data comes in NetCDF binary format for entire globe. I've already extracted the pixel for Ithaca and converted to csv files.

   #ACCESS1-0
   ACCESS <- read.csv("ACCESS1-0_rcp85.csv")
   colnames(ACCESS)<-c("Year", "Day","Precip_mm","Tmax_C","Tmin_C")
   ACCESS$Date = as.Date(ACCESS$Day, origin = as.Date(ISOdate(ACCESS$Year, 1, 1)))
   #Clean up where Tmax < Tmin (this is the actual NASA NEX-GDDP dataset)
   for (i in 1:nrow(ACCESS))
   {if (ACCESS$Tmax_C[i] < ACCESS$Tmin_C[i]) {ACCESS$Tmax_C[i] = ACCESS$Tmin_C[i] + 0.1}}

   #bcc-csm1-1
   bcc <- read.csv("bcc-csm1-1_rcp85.csv")
   colnames(bcc)<-c("Year", "Day","Precip_mm","Tmax_C","Tmin_C")
   bcc$Date = as.Date(bcc$Day, origin = as.Date(ISOdate(bcc$Year, 1, 1)))
   for (i in 1:nrow(bcc))
   {if (bcc$Tmax_C[i] < bcc$Tmin_C[i]) {bcc$Tmax_C[i] = bcc$Tmin_C[i] + 0.1}}

   #BNU-ESM
   BNU <- read.csv("BNU-ESM_rcp85.csv")
   colnames(BNU)<-c("Year", "Day","Precip_mm","Tmax_C","Tmin_C")
   BNU$Date = as.Date(BNU$Day, origin = as.Date(ISOdate(BNU$Year, 1, 1)))
   for (i in 1:nrow(BNU))
   {if (BNU$Tmax_C[i] < BNU$Tmin_C[i]) {BNU$Tmax_C[i] = BNU$Tmin_C[i] + 0.1}}

   #CanESM2
   CanESM <- read.csv("CanESM2_rcp85.csv")
   colnames(CanESM)<-c("Year", "Day","Precip_mm","Tmax_C","Tmin_C")
   CanESM$Date = as.Date(CanESM$Day, origin = as.Date(ISOdate(CanESM$Year, 1, 1)))
   for (i in 1:nrow(CanESM))
   {if (CanESM$Tmax_C[i] < CanESM$Tmin_C[i]) {CanESM$Tmax_C[i] = CanESM$Tmin_C[i] + 0.1}}

   #GDFL-ESM2G
   GFDL <- read.csv("GFDL-ESM2G_rcp85.csv")
   colnames(GFDL)<-c("Year", "Day","Precip_mm","Tmax_C","Tmin_C")
   GFDL$Date = as.Date(GFDL$Day, origin = as.Date(ISOdate(GFDL$Year, 1, 1)))
   for (i in 1:nrow(GFDL))
   {if (GFDL$Tmax_C[i] < GFDL$Tmin_C[i]) {GFDL$Tmax_C[i] = GFDL$Tmin_C[i] + 0.1}}

#Step 2: Plot the projections of annual precipitation total and average annual air temperature for all models

   GCM_Proj_Precip = matrix(nrow = 2099-2014,ncol=5)
   GCM_Proj_Temp = matrix(nrow = 2099-2014,ncol=5)
   for (year in 2015:2099)
   {
     ACCESS_year = ACCESS[which(ACCESS$Year == year),]
     bcc_year = bcc[which(bcc$Year == year),]
     BNU_year = BNU[which(BNU$Year == year),]
     CanESM_year = CanESM[which(CanESM$Year == year),]
     GFDL_year = GFDL[which(GFDL$Year == year),]
     
     GCM_Proj_Precip[year-2014,1] = sum(ACCESS_year$Precip_mm)  
     GCM_Proj_Precip[year-2014,2] = sum(bcc_year$Precip_mm)  
     GCM_Proj_Precip[year-2014,3] = sum(BNU_year$Precip_mm)  
     GCM_Proj_Precip[year-2014,4] = sum(CanESM_year$Precip_mm)  
     GCM_Proj_Precip[year-2014,5] = sum(GFDL_year$Precip_mm)  
     
     GCM_Proj_Temp[year-2014,1] = mean(ACCESS_year$Tmax_C)  
     GCM_Proj_Temp[year-2014,2] = mean(bcc_year$Tmax_C)  
     GCM_Proj_Temp[year-2014,3] = mean(BNU_year$Tmax_C)  
     GCM_Proj_Temp[year-2014,4] = mean(CanESM_year$Tmax_C)  
     GCM_Proj_Temp[year-2014,5] = mean(GFDL_year$Tmax_C)  
   }

   # it is clear from this projection that the ACCESS dataset is an outlier and does not fit the same 
   plot(GCM_Proj_Precip[,1],xlab="Elapsed Years",ylab="Precip (mm/year)")
   lines(GCM_Proj_Precip[,2])
   lines(GCM_Proj_Precip[,3])
   lines(GCM_Proj_Precip[,4])
   lines(GCM_Proj_Precip[,5])

   plot(GCM_Proj_Temp[,1],xlab="Elapsed Years",ylab="Air Temp (deg C)")
   lines(GCM_Proj_Temp[,2])
   lines(GCM_Proj_Temp[,3])
   lines(GCM_Proj_Temp[,4])
   lines(GCM_Proj_Temp[,5])


#Step 3: Run Snowmelt and Lumped_VSA_Model with default parameters with all GCM forcing data
latitudeDegrees = 42.44 #decimal degrees
latitudeRadians<-latitudeDegrees*pi/180 ## latitude in radians

#ACCESS
snowmelt_Access = SnowMelt(Date=ACCESS$Date, precip_mm=ACCESS$Precip_mm, Tmax_C=ACCESS$Tmax_C, Tmin_C=ACCESS$Tmin_C, lat_deg=latitudeDegrees)
Results_Access <- Lumped_VSA_model(dateSeries = ACCESS$Date, P = snowmelt_Access$SnowMelt_mm+snowmelt_Access$Rain_mm, Tmax=ACCESS$Tmax_C, Tmin = ACCESS$Tmin_C, latitudeDegrees=latitudeDegrees, 
                            Depth = 1500, SATper = 0.5, AWCper = 0.17, Tp = 5, albedo = 0.23, StartCond = "avg",
                            BF1 = 1, PETcap = 5, rec_coef = 0.1, Se_min = 78, C1 = 3.1, Ia_coef = 0.05)

#bcc
snowmelt_bcc = SnowMelt(Date=bcc$Date, precip_mm=bcc$Precip_mm, Tmax_C=bcc$Tmax_C, Tmin_C=bcc$Tmin_C, lat_deg=latitudeDegrees)
Results_bcc <- Lumped_VSA_model(dateSeries = bcc$Date, P = snowmelt_bcc$SnowMelt_mm+snowmelt_bcc$Rain_mm, Tmax=bcc$Tmax_C, Tmin = bcc$Tmin_C, latitudeDegrees=latitudeDegrees, 
                                 Depth = 1500, SATper = 0.5, AWCper = 0.17, Tp = 5, albedo = 0.23, StartCond = "avg",
                                 BF1 = 1, PETcap = 5, rec_coef = 0.1, Se_min = 78, C1 = 3.1, Ia_coef = 0.05)

#BNU
snowmelt_BNU = SnowMelt(Date=BNU$Date, precip_mm=BNU$Precip_mm, Tmax_C=BNU$Tmax_C, Tmin_C=BNU$Tmin_C, lat_deg=latitudeDegrees)
Results_BNU <- Lumped_VSA_model(dateSeries = BNU$Date, P = snowmelt_BNU$SnowMelt_mm+snowmelt_BNU$Rain_mm, Tmax=BNU$Tmax_C, Tmin = BNU$Tmin_C, latitudeDegrees=latitudeDegrees, 
                                 Depth = 1500, SATper = 0.5, AWCper = 0.17, Tp = 5, albedo = 0.23, StartCond = "avg",
                                 BF1 = 1, PETcap = 5, rec_coef = 0.1, Se_min = 78, C1 = 3.1, Ia_coef = 0.05)

#Can-ESM
snowmelt_CanESM = SnowMelt(Date=CanESM$Date, precip_mm=CanESM$Precip_mm, Tmax_C=CanESM$Tmax_C, Tmin_C=CanESM$Tmin_C, lat_deg=latitudeDegrees)
Results_CanESM <- Lumped_VSA_model(dateSeries = CanESM$Date, P = snowmelt_CanESM$SnowMelt_mm+snowmelt_CanESM$Rain_mm, Tmax=CanESM$Tmax_C, Tmin = CanESM$Tmin_C, latitudeDegrees=latitudeDegrees, 
                                 Depth = 1500, SATper = 0.5, AWCper = 0.17, Tp = 5, albedo = 0.23, StartCond = "avg",
                                 BF1 = 1, PETcap = 5, rec_coef = 0.1, Se_min = 78, C1 = 3.1, Ia_coef = 0.05)

#GFDL
snowmelt_GFDL = SnowMelt(Date=GFDL$Date, precip_mm=GFDL$Precip_mm, Tmax_C=GFDL$Tmax_C, Tmin_C=GFDL$Tmin_C, lat_deg=latitudeDegrees)
Results_GFDL <- Lumped_VSA_model(dateSeries = GFDL$Date, P = snowmelt_GFDL$SnowMelt_mm+snowmelt_GFDL$Rain_mm, Tmax=GFDL$Tmax_C, Tmin = GFDL$Tmin_C, latitudeDegrees=latitudeDegrees, 
                                 Depth = 1500, SATper = 0.5, AWCper = 0.17, Tp = 5, albedo = 0.23, StartCond = "avg",
                                 BF1 = 1, PETcap = 5, rec_coef = 0.1, Se_min = 78, C1 = 3.1, Ia_coef = 0.05)


#Step 4: Plot all years of Soil Water, Groundwater Storage (Se), and Discharge predicted with ACCESS1 met data
#Qualitatively interpret the  the simulated results from ACCESS1-0_rcp85, what looks right or wrong with the simulated state variables?
   par(mfrow=c(5,1))
   par(mar=c(2.5,5,1.5,1.5))
   plot(snowmelt_Access$Precip_mm[1:365*10],ylab="Precip (mm)")
   plot(snowmelt_Access$SnowWaterEq_mm[1:365*10],ylab="SWE (mm)")
   plot(Results_Access$ET[1:365*10],ylab="ET (mm)")
   plot(Results_Access$SoilWater[1:365*10],ylab="Soil Water (mm)")
   plot(Results_Access$modeled_flow[1:365*10],ylab="Discharge (mm)")

#Step 5: Now plot GFDL-ESM2G_rcp85
#Qualitatively interpret these simulated state variables. What can we say about the quality of the two GCMs for our region?
  par(mfrow=c(5,1))
  par(mar=c(2.5,5,1.5,1.5))
  plot(snowmelt_GFDL$Precip_mm[1:365*10],ylab="Precip (mm)")
  plot(snowmelt_GFDL$SnowWaterEq_mm[1:365*10],ylab="SWE (mm)")
  plot(Results_GFDL$ET[1:365*10],ylab="ET (mm)")
  plot(Results_GFDL$SoilWater[1:365*10],ylab="Soil Water (mm)")
  plot(Results_GFDL$modeled_flow[1:365*10],ylab="Discharge (mm)")


#Step 6: Generate ensemble hydrologic forecasts with GCM / ESM met. projections
# Here we're going to consider all climate models as equally good with arithmetic mean ensemble averaging
# It is definitely not the case that each of these models is equally good, but it's a reasonable starting assumption
# Note: Think about the averaging method. Should we average out the met data and run that through the model?
# Does it make sense to average hydrologic model predictions on each day? GCMs/ESMs are validated seasonally.


#Step 6a. Will Ithaca still get snow in the future?
# - use the model to simulate projections of maximum annual SWE accumulation 
# - what are the physical / ecological processes that are driving this change?
# - note any limitations or assumptions in our the models that might bias these results
  

#Step 6b.  Will future droughts be worse for agriculture?
# - total number of days per year with soil moisture below 180 mm (plant water stress)
# - what are the physical / ecological processes that are driving this change?
# - note any limitations or assumptions in our the models that might bias these results
  

#Step 6c. Will flooding increase in the future?
# - Peak annual discharge
# - what are the physical / ecological processes that are driving this change?
# - note any limitations or assumptions in our the models that might bias these results
  
  
#Step 6d. Do we expect an increase in ET with climate change, [Walter et al 2007]
# - Calculate annual ratio of annual ET to annual precipitation
# - what are the physical / ecological processes that are driving this change?
# - note any limitations or assumptions in our the models that might bias these results
  
