#Ecohydrology Week 13

setwd("~/github/Ecohydrology_Modeling")

library(EcoHydRology)

# Paper used as guide: http://www.science.uwaterloo.ca/~jjgibson/mypdfs/ms_ncelakes.pdf

#Read in Soil and Met Data
VZ = read.csv('SoilWaterBalance.csv')
SH = read.csv('ShaleHills_StemXylem_Isotopes.csv')

#Step 1: Plot the local meteoric water line (LMWL)
plot(VZ$del_O,VZ$del_H) #each dot is a different rain event, the ones low on delO is winter and high is summer

#Give soils some initial isotopic composition for delO and delH
VZ$del_Soil_O[1] = -10
VZ$del_Soil_H[1] = -100

for (i in 2:nrow(VZ))
{
  
  #Step 2: Calculate average daily temperature
  VZ$Tavg_C[i] = (VZ$Tmin_C[i] + VZ$Tmax_C[i])/2

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
  VZ$del_a_O[i] = (VZ$del_O[i] - VZ$e_star[i])
  VZ$del_a_H[i] = (VZ$del_H[i] - VZ$e_star[i])
  
  #Step 3b: Estimate evaporation del H & O
  #delE = ((alpha_star*del_soil) - (relative_humidity*del_a) - epsilon)/(1-relative_humidity+(10^-3)*epsilon_K)
  VZ$del_E_O = ((VZ$alpha_star[i]*VZ$del_Soil_O[i-1]) - (VZ$RelHum[i]*VZ$del_a_O[i]) - VZ$epsilon[i])/(1-VZ$RelHum[i]+(10^(-3))*VZ$e_k[i])
  VZ$del_E_H = ((VZ$alpha_star[i]*VZ$del_Soil_H[i-1]) - (VZ$RelHum[i]*VZ$del_a_H[i]) - VZ$epsilon[i])/(1-VZ$RelHum[i]+(10^(-3))*VZ$e_k[i])
  
  #Step 3c: Use a simple complete mixing approach to calculate the soil water isotopic composition at each time step 
  #What are we assuming about the TWW hypothesis with this approach?
  VZ$del_Soil_O[i] = VZ$del_Soil_O[i-1] + ((VZ$Precip_mm[i] - VZ$Runoff_mm[i])*(VZ$del_O[i])
                                          - (VZ$Recharge_mm[i])*(VZ$del_Soil_O[i])
                                          - (VZ$Evap_mm[i])*(VZ$del_E_O[i])
                                          - (VZ$Transpiration_mm[i])*(VZ$del_Soil_O[i-1])
                                          ) / VZ$SoilWater_mm[i]
    
  VZ$del_Soil_H[i] = VZ$del_Soil_H[i-1] + ((VZ$Precip_mm[i] - VZ$Runoff_mm[i])*(VZ$del_H[i])
                                           - (VZ$Recharge_mm[i])*(VZ$del_Soil_H[i])
                                           - (VZ$Evap_mm[i])*(VZ$del_E_H[i])
                                           - (VZ$Transpiration_mm[i])*(VZ$del_Soil_H[i-1])
                                          ) / VZ$SoilWater_mm[i]                                    
}

#Step 4: Plot Precip (All data) to establish the LMWL, overlay soils for months 9, 10, and 11
#Do soils appear isotopically different from the rainfall?

# Take a subset of soils for 9, 10 and 11
Soil_sub_del_O = VZ$del_Soil_O[VZ$Month > 8 & VZ$Month < 12]
Soil_sub_del_H = VZ$del_Soil_H[VZ$Month > 8 & VZ$Month < 12]  

plot(VZ$del_O, VZ$del_H, xlab = "del 0", ylab = "del H", main = "Soil Water Isotopes in Fall")
abline(lm(VZ$del_H~VZ$del_O), col = "red")
points(Soil_sub_del_O, Soil_sub_del_H, col = "skyblue")
legend( x= "bottomright", 
        legend=c("LMWL", "Precipitation","Soil Water"), 
        col=c("red","black","skyblue"),
        pch=c(20, 21, 21), cex = 0.8)


#Step 5: Overlay all measured plant xylem data with a different color for each species
#Are trees within Shale Hills CZO clearly using shallow soil water or deeper groundwater?
#Do we see a diference in water preference by species?
points(SH$del_O, SH$del_H, col = "green")

species_AS_del_O = SH$del_O[SH$Species == "AS"]
species_QR_del_O = SH$del_O[SH$Species == "QR"]
species_QP_del_O = SH$del_O[SH$Species == "QP"]

species_AS_del_H = SH$del_H[SH$Species == "AS"]
species_QR_del_H = SH$del_H[SH$Species == "QR"]
species_QP_del_H = SH$del_H[SH$Species == "QP"]

plot(VZ$del_O, VZ$del_H, xlab = "del 0", ylab = "del H", main = "Water Preference by Species")
abline(lm(VZ$del_H~VZ$del_O), col = "red")
points(species_AS_del_O, species_AS_del_H, col = "green")
points(species_QR_del_O, species_QR_del_H, col = "plum")
points(species_QP_del_O, species_QP_del_H, col = "slateblue")
legend( x= "bottomright", 
        legend=c("LMWL", "Precipitation","AS","QR","QP"), 
        col=c("red","black","green", "plum", "slateblue"),
        pch=c(20, 21, 21, 21, 21), cex = 0.8)


#Step 6: Create a new plot, replot the LMWL and Soils
#Do we see a difference in tree water preference by season?

# Break up the data into 4 seasons
Fall_del_O = VZ$del_Soil_O[VZ$Month > 9]
Fall_del_H = VZ$del_Soil_H[VZ$Month > 9] 
Winter_del_O = VZ$del_Soil_O[VZ$Month > 0 & VZ$Month < 4]
Winter_del_H = VZ$del_Soil_H[VZ$Month > 0 & VZ$Month < 4] 
Spring_del_O = VZ$del_Soil_O[VZ$Month > 3 & VZ$Month < 7]
Spring_del_H = VZ$del_Soil_H[VZ$Month > 3 & VZ$Month < 7] 
Summer_del_O = VZ$del_Soil_O[VZ$Month > 6 & VZ$Month < 10]
Summer_del_H = VZ$del_Soil_H[VZ$Month > 6 & VZ$Month < 10] 

plot(VZ$del_O, VZ$del_H, xlab = "del 0", ylab = "del H", main = "Soil Water Isotopes by Season")
abline(lm(VZ$del_H~VZ$del_O), col = "red")
points(Fall_del_O, Fall_del_H, col = "sienna")
points(Winter_del_O, Winter_del_H, col = "blue")
points(Spring_del_O, Spring_del_H, col = "violetred")
points(Summer_del_O, Summer_del_H, col = "green")
legend( x= "bottomright", 
        legend=c("LMWL", "Precipitation","Fall","Winter","Spring","Summer"), 
        col=c("red","black","sienna", "blue", "violetred","green"),
        pch=c(20, 21, 21, 21, 21, 21), cex = 0.8)

#Big picture question: What is controlling the local water, hydrology or ecology? 
#Do trees exhibit some ability to draw from different compartments in the landscape?
#Does water availability control the distribution of trees?
