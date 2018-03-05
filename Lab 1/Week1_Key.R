# Ecohydrology 2017
# In Class Assignment 1

# Changing your working directory to a specific folder on your computer
#  setwd("~/Dropbox/Classwork/Ecohydrology") # Mac OSX
#  setwd("\\Users\\Sarah\\Dropbox\\Classwork\\Ecohydrology") # Windows
#  setwd("C:/Ecohydrology/Lecture1/")
setwd("~/Documents/Ecohydrology")

  #declaring a variable
  latitude = 42

#declaring a m x n matrix
  example = matrix(nrow = 5, ncol = 4)

#Filling the second column of a matrix with 0s
  example[,2] = 0

#Filling the first row with 4's
  example[1,] = 4 #it overwrites the 0

#Fill the matrix with 5's
  example[,] = 5

#Calling a function from base R
  total = sum(example)  

# Install and load packages like "EcoHydRology"
  install.packages("EcoHydRology") # Only needs to be done once on your computer
  library(EcoHydRology) #Needs to be done in every new session
  
  # Load a dataset from a file on your computer
  MetData <- read.csv("GameFarmRd_1950-present.csv")

# Looking at the data
  head(MetData)
  summary(MetData)
  summary(MetData$Precip)
  min(MetData$Precip)
  max(MetData$Precip)

# Create a new column in a data frame with a vector operation
# A vector operation performs an action on all values in a vector
  MetData$Date = as.Date(ISOdate(MetData$Year, MetData$Month, MetData$Day))
  
#Convert Precip and Temp to SI units
#Note, this is not a vector operation
  for (i in 1:nrow(MetData))
  {
    MetData$Precip_mm[i] = MetData$Precip[i]*25.4
    MetData$Tmax_C[i] = 5/9*(MetData$Tmax[i]-32)
    MetData$Tmin_C[i] = 5/9*(MetData$Tmin[i]-32)
  }  
  
#Create Time Series Plot of Annual Precipitation Totals
  AnnualPrecip = data.frame(matrix(nrow = 2017-1950))

  for (year in 1950:2016)
  {
  
    AnnualPrecip$Year[year - 1949] = year
    AnnualPrecip$Precip[year - 1949] = sum(MetData[MetData$Year == year,4])
    
  }
  
  plot(AnnualPrecip$Year,AnnualPrecip$Precip, main="Precip Time Series", 
       xlab="Year ", ylab="Precip (mm) ", pch=19)

# Export our met data to a CSV file.
  write.csv(MetData, "MetData_SI.csv")
  
# Load a dataset from USGS using the Ecohydrology function
  FC <- get_usgs_gage(flowgage_id = "04234000", begin_date = "2000-01-01", end_date="2013-12-31")
  
# Look up information about a function
  ?get_usgs_gage  
  
# Produce random numbers
  rand_unif = runif(10000, min=0, max = 1)
  hist(rand_unif)
  
  rand_norm = rnorm(10000, mean=0, sd=1)
  hist(rand_norm)
  
  
  