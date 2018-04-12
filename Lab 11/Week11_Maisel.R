# Ecohydrology 2018
# In Class Assignment 11

setwd("~/github/Ecohydrology_Modeling")

#Step 1: Read in daily temperature data, convert to SI, add a date field
MetData <- read.csv("MetData_Tracer.csv")
MetData$Date = as.Date(ISOdate(MetData$Year, MetData$Month, MetData$Day))

#Step 2: Construct a Water and Tracer Mass Balance Model
#Step 2a: Compute mass of tracer In (mg)
# Convert precipitation from mm to m
MetData$Precip_m = MetData$Precip_mm / 1000
MetData$T_mg = MetData$Cin_mgm * MetData$Precip_m

#Step 2b: Set up the catchment water balance (delta S = Precip - ET - Q)
# Assume an initial catchment storage of 400 mm
# Calculate the mass balance through time
MetData$Q_m = MetData$Q_mm/1000

Si_mm = 400 
Si_m = Si_mm / 1000
MetData$St_mm[1] = Si_mm # initialize with starting condition
for (i in 2:length(MetData$Precip_mm))
{
  MetData$St_mm[i] = MetData$St_mm[i-1] + MetData$Precip_mm[i] - MetData$ET_mm[i] - MetData$Q_mm[i] 
}

MetData$St_m = MetData$St_mm/1000

#Step 2c: Set up the tracer mass balance (delta C_mass = C_mass_in - C_mass_Q - C_Mass_ET)
# Assume ET removes 0 tracer
# Initial Conditions: average precip concentration is ~ 10 mg / m
# Assume an initial catchment storage tracer concentration of 26 mg / m
# Why are we assuming catchment stored concentration is higher than input concentration?

Ci_mgm = 26
Ti_mg = C_mgm * Si_m

# initialize with starting condition
MetData$Ts_mg = Ti_mg 
MetData$Cs_mgm = MetData$Ts_mg / MetData$St_m
MetData$Tout_mg = MetData$Q_m * MetData$Cs_mgm

for (i in 2:length(MetData$Precip_mm))
{
  MetData$Ts_mg[i] = MetData$Ts_mg[i-1] + (MetData$Precip_m[i]*MetData$Cin_mgm[i]) - MetData$T_out_mg[i-1]  
  MetData$Cs_mgm[i] = MetData$Ts_mg[i] / MetData$St_m[i]
  MetData$Tout_mg[i] = MetData$Q_m[i] * MetData$Cs_mgm[i]
}



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