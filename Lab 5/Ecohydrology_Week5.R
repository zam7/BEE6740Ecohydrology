
# Ecohydrology 2017
# In Class Assignment 5

# Load "EcoHydRology" Package
library(EcoHydRology)
# Load "lubridate" Package
library(lubridate)
# Load "ggplot2" Package
library(ggplot2)
# Load "Rmisc" Package
library(Rmisc)

setwd("~/Documents/Ecohydrology")
#Step 1: Read in daily temperature data
FC_MetData <- read.csv("GameFarmRd_1950-present.csv")

#Step 1a: Convert to SI units
#Convert Precip and Temp to SI units, from (in -> cm) and (F -> C)
#Note, this is not a vector operation
for (i in 1:nrow(FC_MetData))
{
  FC_MetData$Precip_mm[i] = FC_MetData$Precip[i]*25.4
  FC_MetData$Tmax_C[i] = 5/9*(FC_MetData$Tmax[i]-32)
  FC_MetData$Tmin_C[i] = 5/9*(FC_MetData$Tmin[i]-32)
}  

#Step 2: Calculate average daily temperature for each record
for (i in 1:nrow(FC_MetData))
{
  FC_MetData$Tavg_C[i] = (FC_MetData$Tmax_C[i] + FC_MetData$Tmin_C[i]) /2
}

#Step 3: Compute julian date and latitude in radians
#Hint: copy from last exercise
FC_MetData$Date = as.Date(ISOdate(FC_MetData$Year, FC_MetData$Month, FC_MetData$Day))
x=as.POSIXlt(FC_MetData$Date, format="Y%b%d")
FC_MetData$JulianDate = x$yday + 1 #julian date 

latitudeDegrees = 42.44 #decimal degrees
latitudeRadians<-latitudeDegrees*pi/180 ## latitude in radians

#Step 4: Compute PET from temperature
#Hint: copy from last exercise
FC_MetData$PET_mm = PET_fromTemp(FC_MetData$JulianDate, FC_MetData$Tmax_C, FC_MetData$Tmin_C, latitudeRadians,
                              albedo = 0.3)*1000 # converts output from m to mm

#Step 5: Crop Growth Stage Modeling - Growing Degree Days
#5a: Calculate growing degree days contributed by each day
# Create the DecidModel and include the dates and average temperature
DecidModel <- data.frame(matrix(nrow = nrow(FC_MetData), ncol = 0))
DecidModel$Date = FC_MetData$Date
DecidModel$Year = FC_MetData$Year
DecidModel$JulianDate = FC_MetData$JulianDate
DecidModel$Tavg_C = FC_MetData$Tavg_C

# Create the ConifModel and include the dates and average temperature
ConifModel <- data.frame(matrix(nrow = nrow(FC_MetData), ncol = 0))
ConifModel$Date = FC_MetData$Date
ConifModel$Year = FC_MetData$Year
ConifModel$JulianDate = FC_MetData$JulianDate
ConifModel$Tavg_C = FC_MetData$Tavg_C

# GDD_max for decidious forest is 2500C
GDD_max_D = 2500 # degC
Tb_C = 1 # Base temperature in degC
DecidModel$GDD = DecidModel$Tavg_C - Tb_C
DecidModel[DecidModel$GDD < 0, 5] = 0 # Sets all the negative values to 0

# GDD_max for coniferous forest is 2500C
GDD_max_C = 2500 # degC
Tb_Con = 0 # Base temperature in degC
ConifModel$GDD = DecidModel$Tavg_C - Tb_Con
ConifModel[ConifModel$GDD < 0, 5] = 0 # Sets all the negative values to 0


#5b: Calculate cummulative growing degree days, and cummulative GDD as a percentage of GDDmax
#Hint: You need a hard reset back to 0 when the calendar year changes
#Hint: The percentage should be limited to 100% even though you can accumulate more GDD than GDDmax in a year (plants are not immortal)
GDD_1 = GDD_max_D * 0
GDD_2 = GDD_max_D * .10
GDD_3 = GDD_max_D * .225
GDD_4 = GDD_max_D * .9

GDD_1C = GDD_max_C * 0
GDD_2C = GDD_max_C * .05
GDD_3C = GDD_max_C * .10
GDD_4C = GDD_max_C * .95

# Deciduous
DecidModel$cGDD[1] = 0
for (i in 2:nrow(DecidModel))
{
  if (DecidModel$JulianDate[i] == 1) # Hard reset to 0 when the calendar year changes
  {
    DecidModel$cGDD[i] = 0
  }
  else (DecidModel$cGDD[i] = DecidModel$GDD[i] + DecidModel$cGDD[i-1])
  
}

DecidModel[DecidModel$cGDD > GDD_max_D, 6] = GDD_max_D

#Confierous
ConifModel$cGDD[1] = 0
for (i in 2:nrow(ConifModel))
{
  if (ConifModel$JulianDate[i] == 1) # Hard reset to 0 when the calendar year changes
  {
    ConifModel$cGDD[i] = 0
  }
  else (ConifModel$cGDD[i] = ConifModel$GDD[i] + ConifModel$cGDD[i-1])
  
}

ConifModel[ConifModel$cGDD > GDD_max_C, 6] = GDD_max_C


# Creating columns for the ratio of cumulative GDD to maximum
# Deciduous
DecidModel$GDDratio = DecidModel$cGDD/GDD_max_D

# Coniferous
ConifModel$GDDratio = ConifModel$cGDD/GDD_max_C

#5c: Assign Kc based on growth stage
#Hint: Calculate alpha then daily Kcb
#Hint: Use a for loop with conditional if statements inside

# Create a column for the plant stage 
# Deciduous
for (i in 1:nrow(DecidModel))
{
  if (DecidModel$cGDD[i] < GDD_2)
  {
    DecidModel$Stage[i] = 1
  }
  else if (DecidModel$cGDD[i] > GDD_2 & DecidModel$cGDD[i] < GDD_3)
  {
    DecidModel$Stage[i] = 2
  }
  else if (DecidModel$cGDD[i] > GDD_3 & DecidModel$cGDD[i] < GDD_4)
  {
    DecidModel$Stage[i] = 3
  }
  else if (DecidModel$cGDD[i] > GDD_4 & DecidModel$cGDD[i] < GDD_max_D)
  {
    DecidModel$Stage[i] = 4
  }
  else 
  {
    DecidModel$Stage[i] = 5
  }
}

# Create a column for the plant stage 
# Coniferous
for (i in 1:nrow(ConifModel))
{
  if (ConifModel$cGDD[i] < GDD_2C)
  {
    ConifModel$Stage[i] = 1
  }
  else if (ConifModel$cGDD[i] > GDD_2C & ConifModel$cGDD[i] < GDD_3C)
  {
    ConifModel$Stage[i] = 2
  }
  else if (ConifModel$cGDD[i] > GDD_3C & ConifModel$cGDD[i] < GDD_4C)
  {
    ConifModel$Stage[i] = 3
  }
  else if (ConifModel$cGDD[i] > GDD_4C & ConifModel$cGDD[i] < GDD_max_C)
  {
    ConifModel$Stage[i] = 4
  }
  else 
  {
    ConifModel$Stage[i] = 5
  }
}

# Create a column for the alphas 
# Deciduous
for (i in 1:nrow(DecidModel))
{
  if (DecidModel$Stage[i] == 1)
  {
    DecidModel$alpha[i] = DecidModel$cGDD[i] / GDD_4
  }
  else if (DecidModel$Stage[i] == 2)
  {
    DecidModel$alpha[i] = (GDD_2/GDD_4) + ((GDD_4 - GDD_2) / (GDD_3 - GDD_2)) * ((DecidModel$cGDD[i] - GDD_2) / GDD_4)
  }
  else if (DecidModel$Stage[i] == 3)
  {
    DecidModel$alpha[i] = 1
  }
  else if (DecidModel$Stage[i] == 4)
  {
    DecidModel$alpha[i] = 1 - 0.6*((DecidModel$cGDD[i] - GDD_4)/(GDD_max_D - GDD_4))
  }
  else
  {
    DecidModel$alpha[i] = 0
  }
}

# Coniferous
for (i in 1:nrow(ConifModel))
{
  if (ConifModel$Stage[i] == 1)
  {
    ConifModel$alpha[i] = ConifModel$cGDD[i] / GDD_4C
  }
  else if (ConifModel$Stage[i] == 2)
  {
    ConifModel$alpha[i] = (GDD_2C/GDD_4C) + ((GDD_4C - GDD_2C) / (GDD_3C - GDD_2C)) * ((ConifModel$cGDD[i] - GDD_2C) / GDD_4C)
  }
  else if (ConifModel$Stage[i] == 3)
  {
    ConifModel$alpha[i] = 1
  }
  else if (ConifModel$Stage[i] == 4)
  {
    ConifModel$alpha[i] = 1 - 0.6*((ConifModel$cGDD[i] - GDD_4C)/(GDD_max_C - GDD_4C))
  }
  else
  {
    ConifModel$alpha[i] = 0
  }
}

# Set values for Kc, the crop coefficient for adjusting PET to reflect the crop capacity to transpire
# Deciduous
Kcmin_D = 0.25
Kcmax_D = 1

for (i in 1:nrow(DecidModel))
{
  DecidModel$Kc[i] = Kcmin_D + DecidModel$alpha[i] * (Kcmax_D - Kcmin_D)
}

# Coniferous
Kcmin_C = 0.9
Kcmax_C = 0.9

for (i in 1:nrow(ConifModel))
{
  ConifModel$Kc[i] = Kcmin_C + ConifModel$alpha[i] * (Kcmax_C - Kcmin_C)
}


#5d: Plot first five years of Kbc and temperature on the same plot, Is there year to year variation in Kc with Temp?
# Deciduous
Kc5_D = DecidModel[0:365*5,]

ggplot(Kc5_D, aes(x = Date)) + 
  geom_line(aes(y = Kc*20, color = "Kc")) +
  geom_line(aes(y = Tavg_C, color = "Temperature")) +
  scale_y_continuous(sec.axis = sec_axis(~./20, name = "Kc")) + 
  scale_color_manual(labels = c("Crop Coefficient","Temperature"), values = c("navy", "rosybrown2")) +
  labs(title = "Crop Coefficient vs. Temperature for a Deciduous Forest in Ithaca, 1950 - 1954",
       x = "Date", y = "Temperature (C)", color = "Parameters\n") 

# Coniferous
Kc5_C = ConifModel[0:365*5,]

ggplot(Kc5_C, aes(x = Date)) + 
  geom_line(aes(y = Kc*20, color = "Kc")) +
  geom_line(aes(y = Tavg_C, color = "Temperature")) +
  scale_y_continuous(sec.axis = sec_axis(~./20, name = "Kc")) + 
  scale_color_manual(labels = c("Crop Coefficient","Temperature"), values = c("navy", "rosybrown2")) +
  labs(title = "Crop Coefficient vs. Temperature for a Coniferous Forest in Ithaca, 1950 - 1954",
       x = "Date", y = "Temperature (C)", color = "Parameters\n") 
  
#Step 6: Compute ETc  = Kc*PET with both methods
# We only did one method

# Deciduous
DecidModel$PET_mm = FC_MetData$PET_mm
DecidModel$PET_c = DecidModel$Kc * DecidModel$PET_mm 

# Deciduous
ConifModel$PET_mm = FC_MetData$PET_mm
ConifModel$PET_c = ConifModel$Kc * ConifModel$PET_mm 

#Step 7: Compare PET_0 and PET_C with a time series plot

# Deciduous
DecidModel5 = DecidModel[1:365*5,]

p1 <- ggplot(DecidModel5, aes(x = Date)) +
  geom_point(aes(y = PET_mm, color = "PET_mm"), alpha = 0.6) + 
  geom_point(aes(y = PET_c, color = "PET_c"), alpha = 0.6) + 
  scale_color_manual(labels = c("PET c", "PET 0"), values = c("royalblue", "magenta")) + 
  labs(title = "PET0 and PETc for a Deciduous Forest in Ithaca, 1950 - 1954", 
       x = "Date", y = "PET (mm)", color = "Deciduous")

# Coniferous
ConifModel5 = ConifModel[1:365*5,]

p2 <- ggplot(ConifModel5, aes(x = Date)) +
  geom_point(aes(y = PET_mm, color = "PET_mm"), alpha = 0.6) + 
  geom_point(aes(y = PET_c, color = "PET_c"), alpha = 0.6) + 
  scale_color_manual(labels = c("PET c", "PET 0"), values = c("royalblue", "magenta")) + 
  labs(title = "PET0 and PETc for a Coniferous Forest in Ithaca, 1950 - 1954", 
       x = "Date", y = "PET (mm)", color = "Coniferous")

multiplot(p1, p2)

#Step 8: Repeat 5b - 7 for coniferous and mixed deciduous forest, how important is the reference crop adjustment for annual mean ET?
#We probably improved ET0 estimation, but what was the tradeoff?