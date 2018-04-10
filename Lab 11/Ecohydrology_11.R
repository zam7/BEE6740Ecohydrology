# Ecohydrology 2018
# In Class Assignment 11

#Step 1: Read in daily temperature data, convert to SI, add a date field
MetData <- read.csv("MetData_Tracer.csv")
MetData$Date = as.Date(ISOdate(MetData$Year, MetData$Month, MetData$Day))

#Step 2: Construct a Water and Tracer Mass Balance Model
#Step 2a: Compute mass of tracer In (mg)

#Step 2b: Set up the catchment water balance (delta S = Precip - ET - Q)
# Assume an initial catchment storage of 400 mm
# Calculate the mass balance through time

#Step 2c: Set up the tracer mass balance (delta C_mass = C_mass_in - C_mass_Q - C_Mass_ET)
# Assume ET removes 0 tracer
# Initial Conditions: average precip concentration is ~ 10 mg / m
# Assume an initial catchment storage tracer concentration of 26 mg / m
# Why are we assuming catchment stored concentration is higher than input concentration?


#Step 3: Compare your simulated catchment tracer to the "observed" do they match?
#Calculate an NSE

#Step 4: Using the water and tracer mass balances, go back and re-estimate the initial catchment storage

#Step 5: Plot Two years of Chloride mass In and Out
#Compare in and out time series
#Which signal has more variability? Why?


#Step 6: Plot two years of catchment Storage chloride concentration (mg / m) and ET
# What effect are we seeing here?

#Are watersheds continuously stirred batch reactors?
#This was the classic assumption, but is being challenged and now largely rejected
#Compare our asumptions to those of McMillan et al (2012)