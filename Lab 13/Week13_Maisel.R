#Ecohydrology Week 13

setwd("~/github/Ecohydrology_Modeling")

library(EcoHydRology)

#Read in Soil and Met Data
VZ = read.csv('SoilWaterBalance.csv')

#Step 1: Plot the local meteoric water line (LMWL)
plot(VZ$del_O,VZ$del_H) #each dot is a different rain event, the ones low on delO is winter and high is summer

#Give soils some initial isotopic composition for delO and delH
VZ$del_Soil_O[1] = -10
VZ$del_Soil_H[1] = -100

for (i in 2:nrow(VZ))
{
  
  #Step 2: Calculate average daily temperature
  for (i in 1:nrow(VZ))
  {
    VZ$Tavg_C[i] = (VZ$Tmin_C[i] + VZ$Tmax_C[i])/2
  }
  #Step 3: Evaporative fractionation and the soil water balance (following Gibson, 2002)
  #Calculate alpha star
  VZ$term1[i] = exp(6.7123 *((10^3)/(VZ$Tavg_C[i] + 273.15))/1000)
  VZ$term2[i] = exp(0.35041*((10^9)/((VZ$Tavg_C[i] + 273.15)^3))/1000)
  VZ$term3[i] = exp(7.685/1000)
  VZ$term4[i] = exp(1.6664*((10^6)/((VZ$Tavg_C[i] + 273.15)^2)/1000))
  VZ$alpha_star[i] = (VZ$term1[i]*VZ$term2[i])/(VZ$term3[i]*VZ$term4[i])
  
  #Calculate estar and epsilon (Equilibrium and kinetic separation)
  VZ$e_star[i] = (VZ$alpha_star[i] - 1)*1000
  VZ$e_k[i] = 1.047
  VZ$epsilon[i] = VZ$e_star[i] + VZ$e_k[i]
  
  #Step 3a: Estimate atmospheric del H & O
  VZ$del_a_O[i] = (VZ$del_O[i] - VZ$e_star[i])/VZ$alpha_star[i]
  VZ$del_a_H[i] = (VZ$del_H[i] - VZ$e_star[i])/VZ$alpha_star[i]
  
  #Step 3b: Estimate evaporation del H & O
  #delE = ((alpha_star*delL) - (relative_humidity*delA) - epsilon)/(1-relative_humidity+(10^-3)*epsilon_K)
  
  #Step 3c: Use a simple complete mixing approach to calculate the soil water isotopic composition at each time step 
  #What are we assuming about the TWW hypothesis with this approach?

}


#Step 4: Plot Precip (All data) to establish the LMWL, overlay soils for months 9, 10, and 11
#Do soils appear isotopically different from the rainfall?

#Step 5: Overlay all measured plant xylem data with a different color for each species
#Are trees within Shale Hills CZO clearly using shallow soil water or deeper groundwater?
#Do we see a diference in water preference by species?

#Step 6: Create a new plot, replot the LMWL and Soils
#Do we see a difference in tree water preference by season?

#Big picture question: What is controlling the local water, hydrology or ecology? 
#Do trees exhibit some ability to draw from different compartments in the landscape?
#Does water availability control the distribution of trees?
