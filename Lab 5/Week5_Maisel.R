# Zoe Maisel
# Ecohydrology 2018
# In Class Assignment 5

# Load "EcoHydRology" Package
library(EcoHydRology)
library(lubridate)
setwd("~/Documents/Ecohydrology")

latitudeDegrees_Ith = 42.44 #decimal degrees
latitudeRadians_Ith<-latitudeDegrees_Ith*pi/180 ## latitude in radians

#Step 1: Read in daily temperature data
#Step 1a: Convert to SI units
#Step 2: Calculate average daily temperature for each record

MetData_Ith <- read.csv("GameFarmRd_1950-present.csv")
for (i in 1:nrow(MetData_Ith))
{
  MetData_Ith$Precip_mm[i] = MetData_Ith$Precip[i]*25.4
  MetData_Ith$Tmax_C[i] = 5/9*(MetData_Ith$Tmax[i]-32)
  MetData_Ith$Tmin_C[i] = 5/9*(MetData_Ith$Tmin[i]-32)
  MetData_Ith$Tavg_C[i] = (MetData_Ith$Tmax_C[i]+MetData_Ith$Tmin_C[i])/2
}  

#Step 3: Compute julian date and latitude in radians
#Hint: copy from last exercise
MetData_Ith$Date = as.Date(ISOdate(MetData_Ith$Year, MetData_Ith$Month, MetData_Ith$Day))
x=as.POSIXlt(MetData_Ith$Date, format="Y%b%d")
MetData_Ith$JulianDate = x$yday + 1

#Step 4: Compute PET from temperature
#Hint: copy from last exercise
MetData_Ith$PET_mm = PET_fromTemp(MetData_Ith$JulianDate, MetData_Ith$Tmax_C, MetData_Ith$Tmin_C, latitudeRadians_Ith, AvgT = (MetData_Ith$Tmax_C + MetData_Ith$Tmin_C)/2, albedo = 0.3)*1000 # the output units from the function was in meters so we needed to multiply by 1000 to get to mm

#Step 5: Crop Growth Stage Modeling - Growing Degree Days
#5a: Calculate growing degree days contributed by each day
# start with coniferous, T baseline = 0
Tb_conif = 0 # deg C

GDD_mod_conf = data.frame(matrix(nrow = length(MetData_Ith$Month), ncol = 0))
GDD_mod_conf$Date = MetData_Ith$Date
GDD_mod_conf$JulianDate = MetData_Ith$JulianDate
GDD_mod_conf$Tavg_C = MetData_Ith$Tavg_C 

#5b: Calculate cummulative growing degree days, and cummulative GDD as a percentage of GDDmax
#Hint: You need a hard reset back to 0 when the calendar year changes
#Hint: The percentage should be limited to 100% even though you can accumulate more GDD than GDDmax in a year (plants are not immortal)
GDD_mod_conf$GDD = MetData_Ith$Tavg_C - Tb_conif # calculate GDD
GDD_mod_conf[GDD_mod_conf$GDD < 0,4] = 0 # set all negative GDD equal to zero

cGDD_max_conf = 2500 # maximum growing degree days 

GDD_mod_conf$cGDD[1] = 0
# need to make a for loop to find cumulative GDD (cGDD)
for(i in 1:nrow(GDD_mod_conf))
{
  if(GDD_mod_conf$JulianDate[i] == 1)
    {
    GDD_mod_conf$cGDD[i] = 0
    } 
  else
    {
    GDD_mod_conf$cGDD[i] = GDD_mod_conf$GDD[i] + GDD_mod_conf$cGDD[i-1]
    }
}

GDD_mod_conf[GDD_mod_conf$cGDD > cGDD_max_conf,5] = cGDD_max_conf # set all values greater than the max to the max

# calculate cGDD as percentage of GDDmax
GDD_mod_conf$percent_cGGD = (GDD_mod_conf$cGDD / cGDD_max_conf) * 100

#5c: Assign Kc based on growth stage
#Hint: Calculate alpha then daily Kcb
#Hint: Use a for loop with conditional if statements inside

# assign values for stages
cGDD1_conf = cGDD_max_conf * 0
cGDD2_conf = cGDD_max_conf * .05
cGDD3_conf = cGDD_max_conf * .10
cGDD4_conf = cGDD_max_conf * .95
cGDD5_conf = cGDD_max_conf * 1.00

for(i in 1:nrow(GDD_mod_conf))
{
  if(GDD_mod_conf$percent_cGGD[i] < cGDD2_conf)
  {
    GDD_mod_conf$stage[i] = 1
  } 
  else if(GDD_mod_conf$percent_cGGD[i] < cGDD3_conf & GDD_mod_conf$percent_cGGD[i] > cGDD2_conf)
  {
    GDD_mod_conf$stage[i] = 2
  }
  else if(GDD_mod_conf$percent_cGGD[i] < cGDD4_conf & GDD_mod_conf$percent_cGGD[i] > cGDD3_conf)
  {
    GDD_mod_conf$stage[i] = 3
  }
  else if(GDD_mod_conf$percent_cGGD[i] < cGDD5_conf & GDD_mod_conf$percent_cGGD[i] > cGDD4_conf)
  {
    GDD_mod_conf$stage[i] = 4
  }
  else
  {
    GDD_mod_conf$stage[i] = 5
  }
}

# create a column for alpha
for (i in 1:nrow(GDD_mod_conf))
{
  if (GDD_mod_conf$stage[i] == 1)
  {
    GDD_mod_conf$alpha[i] = GDD_mod_conf$cGDD[i] / cGDD4_conf
  }
  else if (GDD_mod_conf$stage[i] == 2)
  {
    GDD_mod_conf$alpha[i] = (cGDD2_conf/cGDD4_conf) + ((cGDD4_conf - cGDD2_conf) / (cGDD3_conf - cGDD2_conf)) * ((GDD_mod_conf$cGDD[i] - cGDD2_conf) / cGDD4_conf)
  }
  else if (GDD_mod_conf$stage[i] == 3)
  {
    GDD_mod_conf$alpha[i] = 1
  }
  else if (GDD_mod_conf$stage[i] == 4)
  {
    GDD_mod_conf$alpha[i] = 1 - 0.6*((GDD_mod_conf$cGDD[i] - cGDD4_conf)/(GDD_max_C - cGDD4_conf))
  }
  else
  {
    GDD_mod_conf$alpha[i] = 0
  }
}

# Kc min and max values for coniferous 
Kcmin_conf = 0.9
Kcmax_conf = 0.9

for(i in 1:nrow(GDD_mod_conf))
  {
  GDD_mod_conf$Kc[i] = Kcmin_conf + GDD_mod_conf$alpha[i]*(Kcmax_conf - Kcmin_conf)
  }


#5d: Plot first five years of Kbc and temperature on the same plot 
#Is there year to year variation in Kc with Temp?
par(mar=c(5, 4, 4, 6) + 0.1)
## Plot first set of data and draw its axis
plot(GDD_mod_conf$Date[(0:365*5)], GDD_mod_conf$Tavg_C[(0:365*5)], pch=16,  
     ylim=c(-20,25), xlab="", ylab="", 
     type="b",col="black", main="Conifer Forest")
mtext("Tavg (C)",side=2,line=2.5)
box()
## Allow a second plot on the same graph
par(new=TRUE)
## Plot the second plot and put axis scale on right
plot(GDD_mod_conf$Date[(0:365*5)], GDD_mod_conf$Kc[(0:365*5)], pch=19,  xlab="", ylab="", 
     ylim=c(-1,2), 
     axes=FALSE, type="b", col="red")
## a little farther out (line=4) to make room for labels
mtext("Kc",side=4,col="red",line=4) 
axis(4, ylim=c(-1,2), col="red",col.axis="red",las=1)
## Draw the time axis
mtext("Date (Years)",side=1,col="black",line=2.5)

#Step 6: Compute P ETc  = Kc*PET
GDD_mod_conf$PETc = GDD_mod_conf$Kc*MetData_Ith$PET_mm

#Step 7: Compare PET_0 and PET_C with a time series plot
plot(GDD_mod_conf$Date[(0:365*5)], MetData_Ith$PET_mm[(0:365*5)], xlab = "Date", ylab = "PET (mm)", col = "red", main = "Coniferous")
points(GDD_mod_conf$Date[(0:365*5)], GDD_mod_conf$PETc[(0:365*5)], col = "black", pch = 24)
legend( x= "topright", y=0.92, 
        legend=c("PET_0 (mm)","PET_C (mm)"), 
        col=c("red","black"),
        pch=c(21, 24))

#Step 8: Repeat 5b - 7 for coniferous and mixed deciduous forest, how important is the reference crop adjustment for annual mean ET?
#We probably improved ET0 estimation, but what was the tradeoff?

############################ Mixed Deciduous
#Step 5: Crop Growth Stage Modeling - Growing Degree Days
#5a: Calculate growing degree days contributed by each day
# Deciduous 
Tb_conif = 1 # deg C

GDD_mod_decid = data.frame(matrix(nrow = length(MetData_Ith$Month), ncol = 0))
GDD_mod_decid$Date = MetData_Ith$Date
GDD_mod_decid$JulianDate = MetData_Ith$JulianDate
GDD_mod_decid$Tavg_C = MetData_Ith$Tavg_C 

#5b: Calculate cummulative growing degree days, and cummulative GDD as a percentage of GDDmax
#Hint: You need a hard reset back to 0 when the calendar year changes
#Hint: The percentage should be limited to 100% even though you can accumulate more GDD than GDDmax in a year (plants are not immortal)
GDD_mod_decid$GDD = MetData_Ith$Tavg_C - Tb_conif # calculate GDD
GDD_mod_decid[GDD_mod_decid$GDD < 0,4] = 0 # set all negative GDD equal to zero

cGDD_max_decid = 2500 # maximum growing degree days 

GDD_mod_decid$cGDD[1] = 0
# need to make a for loop to find cumulative GDD (cGDD)
for(i in 1:nrow(GDD_mod_decid))
{
  if(GDD_mod_decid$JulianDate[i] == 1)
  {
    GDD_mod_decid$cGDD[i] = 0
  } 
  else
  {
    GDD_mod_decid$cGDD[i] = GDD_mod_decid$GDD[i] + GDD_mod_decid$cGDD[i-1]
  }
}

GDD_mod_decid[GDD_mod_decid$cGDD > cGDD_max_decid,5] = cGDD_max_decid # set all values greater than the max to the max

# calculate cGDD as percentage of GDDmax
GDD_mod_decid$percent_cGGD = (GDD_mod_decid$cGDD / cGDD_max_decid) * 100

#5c: Assign Kc based on growth stage
#Hint: Calculate alpha then daily Kcb
#Hint: Use a for loop with conditional if statements inside

# assign values for stages
cGDD1_conf = cGDD_max_decid * 0
cGDD2_conf = cGDD_max_decid * .1
cGDD3_conf = cGDD_max_decid * .225
cGDD4_conf = cGDD_max_decid * .90
cGDD5_conf = cGDD_max_decid * 1.00

for(i in 1:nrow(GDD_mod_decid))
{
  if(GDD_mod_decid$percent_cGGD[i] < cGDD2_conf)
  {
    GDD_mod_decid$stage[i] = 1
  } 
  else if(GDD_mod_decid$percent_cGGD[i] < cGDD3_conf & GDD_mod_decid$percent_cGGD[i] > cGDD2_conf)
  {
    GDD_mod_decid$stage[i] = 2
  }
  else if(GDD_mod_decid$percent_cGGD[i] < cGDD4_conf & GDD_mod_decid$percent_cGGD[i] > cGDD3_conf)
  {
    GDD_mod_decid$stage[i] = 3
  }
  else if(GDD_mod_decid$percent_cGGD[i] < cGDD5_conf & GDD_mod_decid$percent_cGGD[i] > cGDD4_conf)
  {
    GDD_mod_decid$stage[i] = 4
  }
  else
  {
    GDD_mod_decid$stage[i] = 5
  }
}

# create a column for alpha
for (i in 1:nrow(GDD_mod_decid))
{
  if (GDD_mod_decid$stage[i] == 1)
  {
    GDD_mod_decid$alpha[i] = GDD_mod_decid$cGDD[i] / cGDD4_conf
  }
  else if (GDD_mod_decid$stage[i] == 2)
  {
    GDD_mod_decid$alpha[i] = (cGDD2_conf/cGDD4_conf) + ((cGDD4_conf - cGDD2_conf) / (cGDD3_conf - cGDD2_conf)) * ((GDD_mod_decid$cGDD[i] - cGDD2_conf) / cGDD4_conf)
  }
  else if (GDD_mod_decid$stage[i] == 3)
  {
    GDD_mod_decid$alpha[i] = 1
  }
  else if (GDD_mod_decid$stage[i] == 4)
  {
    GDD_mod_decid$alpha[i] = 1 - 0.6*((GDD_mod_decid$cGDD[i] - cGDD4_conf)/(GDD_max_C - cGDD4_conf))
  }
  else
  {
    GDD_mod_decid$alpha[i] = 0
  }
}




# Kc min and max values for deciduous 
Kcmin_decid = 0.25
Kcmax_decid = 1

for(i in 1:nrow(GDD_mod_decid))
{
  GDD_mod_decid$Kc[i] = Kcmin_decid + GDD_mod_decid$alpha[i]*(Kcmax_decid - Kcmin_decid)
}


#5d: Plot first five years of Kbc and temperature on the same plot 
#Is there year to year variation in Kc with Temp?
par(mar=c(5, 4, 4, 6) + 0.1)
## Plot first set of data and draw its axis
plot(GDD_mod_decid$Date[(0:365*5)], GDD_mod_decid$Tavg_C[(0:365*5)], pch=16,  
     ylim=c(-20,25), xlab="", ylab="", 
     type="b",col="black", main="Deciduous Forest")
mtext("Tavg (C)",side=2,line=2.5)
box()
## Allow a second plot on the same graph
par(new=TRUE)
## Plot the second plot and put axis scale on right
plot(GDD_mod_decid$Date[(0:365*5)], GDD_mod_decid$Kc[(0:365*5)], pch=19,  xlab="", ylab="", 
     ylim=c(-1,2), 
     axes=FALSE, type="b", col="red")
## a little farther out (line=4) to make room for labels
mtext("Kc",side=4,col="red",line=4) 
axis(4, ylim=c(-1,2), col="red",col.axis="red",las=1)
## Draw the time axis
mtext("Date (Years)",side=1,col="black",line=2.5)  

#Step 6: Compute ETc  = Kc*PET
GDD_mod_decid$PETc = GDD_mod_decid$Kc*MetData_Ith$PET_mm

#Step 7: Compare PET_0 and PET_C with a time series plot
plot(GDD_mod_decid$Date[(0:365*5)], MetData_Ith$PET_mm[(0:365*5)], xlab = "Date", ylab = "PET (mm)", col = "red", main = "Deciduous")
points(GDD_mod_decid$Date[(0:365*5)], GDD_mod_decid$PETc[(0:365*5)], col = "black", pch = 24)
legend( x= "topright", y=0.92, 
        legend=c("PET_0 (mm)","PET_C (mm)"), 
        col=c("red","black"),
        pch=c(21, 24))
