# Ecohydrology 2016
# Week 3
# What portion of the precipitation is lost as evapotranspiration in a watershed? 

# Load "EcoHydRology" Package
library(EcoHydRology)
library(lubridate)

# Step 1: Set your working directory to the folder where you've stored your precipitation data and load that data into the R environment.
setwd("~/Documents/Ecohydrology")
FC_Data <- read.csv("GameFarmRd_1950-present.csv")

# Step 2: Download USGS streamflow data using the EcoHydRology package
# Fall Creek, Ithaca, NY: "04234000"
FC = get_usgs_gage(flowgage_id = "04234000", begin_date = "1950-01-01", end_date="2016-12-31")

# Step 3: Find the size of the watershed for a USGS Streamgage, Convert streamflow to mm / day
# Tip: get_usgs_gage function returns area in square kilometers.
area_FC = FC$area #km^2
FC_flowdata = data.frame(matrix(nrow = length(FC$flowdata$flow),ncol = 0)) # make data into a dataframe to allow for easier analysis
FC_flowdata$Date = FC$flowdata$date # this brings my date column into the correct data frame so I can look at it
FC_flowdata$Year = year(FC_flowdata$Date) # this takes just the year
FC_flowdata$flowrate_mmperd = FC$flowdata$flow/area_FC/(1000) # we get the data in units of m^3/day and we convert it to mm/day

#Step 4: calculate the annual total precipitation and streamflow

#Convert Precip to SI units
for (i in 1:nrow(FC_Data))
{
  FC_Data$Precip_mm[i] = FC_Data$Precip[i]*25.4
}  

# Get data into annual form
Annual_FC_Data = data.frame(matrix(nrow = 2017-1950, ncol = 0))
for (year in 1950:2016)
{
  Annual_FC_Data$Year[year - 1949] = year # populates AnnualData with years
  Annual_FC_Data$Precip_mm[year - 1949] = sum(FC_Data[FC_Data$Year == year,7]) # precipitation in mm
  Annual_FC_Data$Streamflow_mmpery[year - 1949] = sum(FC_flowdata[FC_flowdata$Year == year,3]) # stream depth in mm/d
}

# Step 5: Calculate the annual evapotranspiration for each year
# Storage = precipitation - evapotranspiration - streamflow
# Assume water storage in a watershed is neglible over a year (storage = 0)
# Evapotranspiration = precipitation - streamflow

Annual_FC_Data$ET = Annual_FC_Data$Precip_mm - Annual_FC_Data$Streamflow_mmpery

# Step 6: Calculate and plot the annual portion of precipitation that is evapotranspiration 
#Plot the trends of Precip, ET, and ET Ratio

Annual_FC_Data$ETratio = Annual_FC_Data$ET/Annual_FC_Data$Precip_mm

# Steps 7 and 8: Fit a linear model to the data with x=year and y=ET
# Is there a significant trend?

# Plot Precip and Year with linear model
plot.new()
plot(Annual_FC_Data$Year,Annual_FC_Data$Precip_mm, main="Ithaca Precipitation Annual Series", 
     xlab="Year ", ylab="Precip (mm) ")
abline(lm(Annual_FC_Data$Precip_mm ~ Annual_FC_Data$Year))
summary(lm(Annual_FC_Data$Precip_mm ~ Annual_FC_Data$Year))
p_value_Precip_FC = summary(lm(Annual_FC_Data$Precip_mm ~ Annual_FC_Data$Year))$coefficients[2,4]
# The p-value is 0.1562. This is greater than the chosen p-value of 0.1 so it is not statistically significant.

# Plot ET and Year with linear model
plot.new()
plot(Annual_FC_Data$Year,Annual_FC_Data$ET, main="Ithaca ET Annual Series", 
     xlab="Year ", ylab="ET (mm) ")
abline(lm(Annual_FC_Data$ET ~ Annual_FC_Data$Year))
summary(lm(Annual_FC_Data$ET ~ Annual_FC_Data$Year))
p_value_ET_FC = summary(lm(Annual_FC_Data$ET ~ Annual_FC_Data$Year))$coefficients[2,4]
# The p-value is 0.1844. This is greater than the chosen p-value of 0.1 so it is not statistically significant.

# Plot ET/P and Year with linear model
plot.new()
plot(Annual_FC_Data$Year,Annual_FC_Data$ETratio, main="Ithaca ET/P Annual Series", 
     xlab="Year ", ylab="ET/P")
abline(lm(Annual_FC_Data$ETratio ~ Annual_FC_Data$Year))
summary(lm(Annual_FC_Data$ETratio ~ Annual_FC_Data$Year))
p_value_ETratio_FC = summary(lm(Annual_FC_Data$ETratio ~ Annual_FC_Data$Year))$coefficients[2,4]
# The p-value is 0.9868. This is greater than the chosen p-value of 0.1 so it is not statistically significant.

#################################################################################################################
# Step 9: Analysis for Albuquerque, New Mexico
# Obtained precipitation data from NOAA "Global Summary of the Year" for the Albuquerque Airport Station because it had the correct date range and 100% coverage
Annual_RG_Data <- read.csv("AlbuquerquePrecipData.csv") # gives annual precip in inches/year

# Step 2: Download USGS streamflow data for Rio Grande, Albuquerque, NM 
# https://waterdata.usgs.gov/nwis/uv?site_no=08330000
# Albuquerque, New Mexico: "08330000"
RG = get_usgs_gage(flowgage_id = "08330000", begin_date = "1950-01-01", end_date="2016-12-31")

# Step 3: Find the size of the watershed for a USGS Streamgage, Convert streamflow to mm / day
# Tip: get_usgs_gage function returns area in square kilometers.
area_RG = RG$area #km^2
RG_flowdata = data.frame(matrix(nrow = length(RG$flowdata$flow),ncol = 0)) # make data into a dataframe to allow for easier analysis
RG_flowdata$Date = RG$flowdata$date # this brings my date column into the correct data frame so I can look at it
RG_flowdata$Year = year(RG_flowdata$Date) # this takes just the year
RG_flowdata$flowrate_mmperd = RG$flowdata$flow/area_RG/(1000) # we get the data in units of m^3/day and we convert it to mm/day

#Step 4: calculate the annual total precipitation and streamflow

#Convert Precip to SI units
for (i in 1:nrow(Annual_RG_Data))
{
  Annual_RG_Data$Precip_mm[i] = Annual_RG_Data$PRCP[i]*25.4
}  

# Get streamflow annuals for Rio Grande and put in the annuals data frame for Albuquerque
for (year in 1950:2016)
{
  Annual_RG_Data$Streamflow_mmpery[year - 1949] = sum(RG_flowdata[RG_flowdata$Year == year,3]) # stream depth in mm/d
}

# Step 5: Calculate the annual evapotranspiration for each year
# Storage = precipitation - evapotranspiration - streamflow
# Assume water storage in a watershed is neglible over a year (storage = 0)
# Evapotranspiration = precipitation - streamflow

Annual_RG_Data$ET = Annual_RG_Data$Precip_mm - Annual_RG_Data$Streamflow_mmpery

# Step 6: Calculate and plot the annual portion of precipitation that is evapotranspiration 
#Plot the trends of Precip, ET, and ET Ratio

Annual_RG_Data$ETratio = Annual_RG_Data$ET/Annual_RG_Data$Precip_mm

# Steps 7 and 8: Fit a linear model to the data with x=year and y=ET
# Is there a significant trend?

# Plot Precip and Year with linear model
plot.new()
plot(Annual_RG_Data$DATE,Annual_RG_Data$Precip_mm, main="Albuquerque Precipitation Annual Series", 
     xlab="Year ", ylab="Precip (mm) ")
abline(lm(Annual_RG_Data$Precip_mm ~ Annual_RG_Data$DATE))
summary(lm(Annual_RG_Data$Precip_mm ~ Annual_RG_Data$DATE))

p_value_Precip_RG = summary(lm(Annual_RG_Data$Precip_mm ~ Annual_RG_Data$DATE))$coefficients[2,4]
# The p-value is 0.04772. This is less than the chosen p-value of 0.1 so it is statistically significant.

# Plot ET and Year with linear model
plot.new()
plot(Annual_RG_Data$DATE,Annual_RG_Data$ET, main="Albuquerque ET Annual Series", 
     xlab="Year ", ylab="ET (mm) ")
abline(lm(Annual_RG_Data$ET ~ Annual_RG_Data$DATE))
summary(lm(Annual_RG_Data$ET ~ Annual_RG_Data$DATE))
p_value_ET_RG = summary(lm(Annual_RG_Data$ET ~ Annual_RG_Data$DATE))$coefficients[2,4]
# The p-value is 0.04211 This is less than the chosen p-value of 0.1 so it is statistically significant.

# Plot ET/P and Year with linear model
plot.new()
plot(Annual_RG_Data$DATE,Annual_RG_Data$ETratio, main="Albuquerque ET/P Annual Series", 
     xlab="Year ", ylab="ET/P")
abline(lm(Annual_RG_Data$ETratio ~ Annual_RG_Data$DATE))
summary(lm(Annual_RG_Data$ETratio ~ Annual_RG_Data$DATE))
p_value_ETratio_RG = summary(lm(Annual_RG_Data$ETratio ~ Annual_RG_Data$DATE))$coefficients[2,4]
# The p-value is 0.9262 This is greater than the chosen p-value of 0.1 so it is not statistically significant.

