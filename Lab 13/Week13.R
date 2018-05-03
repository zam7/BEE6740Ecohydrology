#Ecohydrology Week 13
library(EcoHydRology)

#Read in Soil and Met Data
VZ = read.csv('C:/Ecohydrology/Lecture13/SoilWaterBalance.csv')

#Step 1: Plot the local meteoric water line (LMWL)
plot(VZ$del_O,VZ$del_H)

#Give soils some initial isotopic composition for delO and delH
VZ$del_Soil_O[1] = -10
VZ$del_Soil_H[1] = -100

for (i in 2:nrow(VZ))
{
  
  #Step 2: Calculate average daily temperature

  #Step 3: Evaporative fractionation and the soil water balance (following Gibson, 2002)
  #Calculate alpha star
  term1 = exp(6.7123 *((10^3)/(Ta + 273.15))/1000)
  term2 = exp(0.35041*((10^9)/((Ta + 273.15)^3))/1000)
  term3 = exp(7.685/1000)
  term4 = exp(1.6664*((10^6)/((Ta + 273.15)^2)/1000))
  alpha_star = (term1*term2)/(term3*term4)
  
  #Calculate estar and epsilon (Equilibrium and kinetic separation)
  e_star = (alpha_star - 1)*1000
  e_k = 1.047
  epsilon = e_star + e_k
  
  #Step 3a: Estimate atmospheric del 18O

  #Step 3b: Estimate evaporation del 18O

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
