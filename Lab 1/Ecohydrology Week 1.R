# Ecohydrology 2017
# In Class Assignment 1

# Changing your working directory to a specific folder on your computer
setwd("~/Documents/Ecohydrology") # Setting the working directory (if you don'tknow how to set find the path for the working directory, go to session button up top and manually set the path)

#declaring a variable
latitude = 42
correction = 1
latitude = latitude + correction

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
#hint: read.csv
MetData <- read.csv("GameFarmRd_1950-present.csv")

# Looking at the data
head(MetData)
summary(MetData)
summary(MetData$Precip)
min(MetData$Precip)
max(MetData$Precip)
typeof(MetData$Year)

# Create a new date column in a data frame with a vector operation
#hint ?ISOdate
#super hint: as.Date(ISOdate(Year, Month, Day))
#ISO date takes out the values that are relevant to our date
#as.Date formats it in a readable way as a date
MetData$Date = as.Date(ISOdate(MetData$Year, MetData$Month, MetData$Day))

#Convert Precip and Temp to SI units
#execute this command individually for each record, try this inside of a "for loop"
#useful if there is a need to change the way that you are treating data differently halfway through a set 
#C = 5/9*(F - 32)   
for (i in 1:nrow(MetData$Date))
{
  MetData$Precip_mm[i] = MetData$Precip[i]*25.4 #to convert to SI precip
  MetData$Tmax_C[i] = (5/9)*(MetData$Tmax-32) #to convert to SI temp
  MetData$Tmin_C[i] = (5/9)*(MetData$Tmin-32) #to convert to SI temp
}

  
#Create Time Series Plot of Annual Precipitation Totals
#Summarize annual precipitation totals by year
#- can be done inside of a for loop, or plenty of other ways
#?plot()

#Subsetting Data
#Take the previous dataset and create a new record containing only 1990 to present
#?which

# Export our met data to a CSV file.
#?write.csv

# Load a dataset from USGS using the Ecohydrology function
#?get_usgs_gage

# Produce random draws from a uniform distribution a = 0, b = 1 and create a histogram plot
#Try 100 random samples, then try 10000
#?runif
#?hist

#Histogram plot of random draws from a standard normal distribution (mean = 0, stdev = 1)
#?rnorm
