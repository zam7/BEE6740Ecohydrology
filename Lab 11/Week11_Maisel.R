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
MetData$Tin_obs_mg = MetData$Cin_mgm * MetData$Precip_m

#Step 2b: Set up the catchment water balance (delta S = Precip - ET - Q)
# Assume an initial catchment storage of 400 mm
# Calculate the mass balance through time
MetData$Q_m = MetData$Q_mm/1000

Si_mm = 370 
Si_m = Si_mm / 1000
MetData$St_mm[1] = Si_mm # initialize with starting condition
for (i in 2:length(MetData$Precip_mm))
{
  MetData$St_mm[i] = MetData$St_mm[i-1] + MetData$Precip_mm[i] - MetData$ET_mm[i] - MetData$Q_mm[i] 
}

MetData$St_m = MetData$St_mm/1000

plot(MetData$St_mm)

#Step 2c: Set up the tracer mass balance (delta C_mass = C_mass_in - C_mass_Q - C_Mass_ET)
# Assume ET removes 0 tracer
# Initial Conditions: average precip concentration is ~ 10 mg / m
# Assume an initial catchment storage tracer concentration of 26 mg / m
# Why are we assuming catchment stored concentration is higher than input concentration?

Ci_mgm = 26
Ti_mg = Ci_mgm * Si_m

# Assume mixing in the catchement
# initialize with starting condition
MetData$Ts_mg = Ti_mg 
MetData$Cs_mgm = MetData$Ts_mg / MetData$St_m
MetData$Tout_mix_mg = MetData$Q_m * MetData$Cs_mgm

for (i in 2:length(MetData$Precip_mm))
{
  MetData$Ts_mg[i] = MetData$Ts_mg[i-1] + (MetData$Precip_m[i]*MetData$Cin_mgm[i]) - MetData$Tout_mix_mg[i-1]  
  MetData$Cs_mgm[i] = MetData$Ts_mg[i] / MetData$St_m[i]
  MetData$Tout_mix_mg[i] = MetData$Q_m[i] * MetData$Cs_mgm[i]
}

# Assume no mixing, calculate Tout_nomix_mg
MetData$Tout_nomix_mg = MetData$Q_m * MetData$Cin_mgm

# Convert Tout values into Cout values
MetData$Cout_mix_mgm = MetData$Tout_mix_mg / MetData$Q_m
MetData$Cout_nomix_mgm = MetData$Tout_nomix_mg / MetData$Q_m

# Replace all NaN for zero flow with 0
MetData[is.na(MetData)] <- 0

# Plot the observed vs simulated values for mass and concentration

# Observed vs simulated values for concentration for mixing and no mixing conditions
par(mfrow=c(2,1))
par(mar=c(4,4,2.5,2.5))
plot(MetData$Date[1:(5*365)], MetData$Cout_mgm[1:(5*365)], main = "Assumed Mixing in Watershed", xlab = "Date (years)", ylab = "Tracer Concentration (mg/m)")
lines(MetData$Date[1:(5*365)], MetData$Cout_mix_mgm[1:(5*365)], col = "red")
legend( x= "topright", 
        legend=c("Observed","Modeled"), 
        col=c("black","red"),
        pch=c(20, 21), cex = 0.5)
plot(MetData$Date[1:(5*365)], MetData$Cout_mgm[1:(5*365)], main = "Assumed No Mixing in Watershed", xlab = "Date (years)", ylab = "Tracer Concentration (mg/m)")
lines(MetData$Date[1:(5*365)], MetData$Cout_nomix_mgm[1:(5*365)], col = "red")
legend( x= "topright", 
        legend=c("Observed","Modeled"), 
        col=c("black","red"),
        pch=c(20, 21), cex = 0.5)

# Observed vs simulated values for mass for mixing and no mixing conditions
  # Calculate Tout observed using the Cout and Q
  MetData$Tout_obs_mg = MetData$Cout_mgm * MetData$Q_m

  par(mfrow=c(2,1))
  par(mar=c(4,4,2.5,2.5))
plot(MetData$Date[1:(5*365)], MetData$Tout_obs_mg[1:(5*365)], main = "Assumed Mixing in Watershed", xlab = "Date (years)", ylab = "Effluent Chloride Mass (mg)")
lines(MetData$Date[1:(5*365)], MetData$Tout_mix_mg[1:(5*365)], col = "red")
legend( x= "topright", 
        legend=c("Observed","Modeled"), 
        col=c("black","red"),
        pch=c(20, 21), cex = 0.5)

plot(MetData$Date[1:(5*365)], MetData$Tout_obs_mg[1:(5*365)], main = "Assumed No Mixing in Watershed", xlab = "Date (years)", ylab = "Effluent Chloride Mass (mg)")
lines(MetData$Date[1:(5*365)], MetData$Tout_nomix_mg[1:(5*365)], col = "red")
legend( x= "topright", 
        legend=c("Observed","Modeled"), 
        col=c("black","red"),
        pch=c(20, 21), cex = 0.5)

#Step 4: Using the water and tracer mass balances, go back and re-estimate the initial catchment storage
# The inital guess of 400 mg/m appeared to be too high, so the value was re-estimated to be 370 mg/m
# Both concentration and mass plots show that the assumed mixing in the watershed condition matched the observed data best,
# so the mixing condition is used 

#Step 5: Plot Two years of Chloride mass In and Out # for the mixing condition
#Compare in and out time series
#Which signal has more variability? Why?
plot(MetData$Date[1:(2*365)], MetData$Tin_obs_mg[1:(2*365)], xlab = "Date (years)", ylab = "Influent Chloride Mass (mg)", main = "Chlorine Influent and Effluent")
lines(MetData$Date[1:(2*365)], MetData$Tout_mix_mg[1:(2*365)], xlab = "Date (years)", ylab = "Effluent Chloride Mass (mg)", col = "red")
legend( x= "topright", 
        legend=c("Influent","Effluent"), 
        col=c("black","red"),
        pch=c(20, 21), cex = 0.5)

#Step 6: Plot two years of catchment Storage chloride concentration (mg / m) and ET
# What effect are we seeing here?
par(mfrow=c(2,1))
par(mar=c(4,4,2.5,2.5))
plot(MetData$Date[1:(2*365)], MetData$Cs_mgm[1:(2*365)], xlab = "Date (years)", ylab = "Stored Chloride Mass (mg/m)")
plot(MetData$Date[1:(2*365)], MetData$ET_mm[1:(2*365)], xlab = "Date (years)", ylab = "Evapotranspiration (mm)")

# we expect to see that as ET increases, concentration increases because the evaporated water is leaving behind a higher concentration of water

#Are watersheds continuously stirred batch reactors?
#This was the classic assumption, but is being challenged and now largely rejected
#Compare our asumptions to those of McMillan et al (2012)


## Graphical representation of different expectations for mixing or nonmixing conditions
x=seq(0,10,0.1)
par(mfrow=c(3,1))
par(mar=c(4,4,2.5,2.5))
  
  # Time-delay, no mixing
  xpfr=x+140
  plot(1*sin(x), main = "Time-delay, no mixing PFR", ylab = "Concentration", xlab = "")
  lines(1*sin(xpfr), col = "red")
  legend( x= "topright",
          legend=c("Effluent","Influent"), 
          col=c("black","red"),
          pch=c(21, 20), cex = 0.5)
  
  # Short-circuiting, nonzero storage
  xsc=x+0.5
  plot(1*sin(x), main = "Short-circuiting, no mixing", ylab = "Concentration", xlab = "")
  lines(1*sin(xsc), col = "red")
  legend( x= "topright",
          legend=c("Effluent","Influent"), 
          col=c("black","red"),
          pch=c(21, 20), cex = 0.5)
  
  # Mixing
  xsc=x+100
  plot(0.3*sin(x), main = "Mixing, CSTR", ylim = c(-1,1), ylab = "Concentration", xlab = "Time")
  lines(1*sin(xsc), col = "red")
  legend( x= "topright",
          legend=c("Effluent","Influent"), 
          col=c("black","red"),
          pch=c(21, 20), cex = 0.5)

