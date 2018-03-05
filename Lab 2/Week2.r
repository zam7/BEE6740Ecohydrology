# Ecohydrology 2016
# In Class Assignment 2

setwd("~/Documents/Ecohydrology")
library(EcoHydRology)

# USGS Streamgages
# 1. Fall Creek, Ithaca, NY: "04234000"
# 2. Little Sioux River, Linn Gorge, IA: "06605850"
# 3. White Salmon River, Underwood, WA: "14123500" The code uses this river in the analysis
# 4. White Oak Creek, Georgetown, OH: "03238500"
# 5. Pantano Wash, Vail, AZ: "09484600"

# Baseflow Separation -----------------------------------------------------
# Step 1: Choose a location and load data using the get_usgs_gage function in
#   the EcoHydRology package for "1990-01-01" through "2016-12-31"
#   Check flow units and convert to cubic meters per second
WSR = get_usgs_gage(flowgage_id = "14123500", begin_date = "1990-01-01", end_date="2016-12-31") # chose White Salmon River
flow_CMS = (WSR$flowdata$flow/(24*60*60)) # we get the data in units of m^3/day and we convert it to m^3/s

# Step 2: Calculate baseflow with the BaseflowSeparation function using the 
#   streamflow data from Step 1. Look at the data with the head() and summary()
#   functions from last class.
# ?BaseflowSeparation #use this command to read more about the function
#   Hint: You will need to select the exact column ("flow") in the data frame
#   ("flowdata") inside the list generated in Step 1.
bfs = BaseflowSeparation(flow_CMS,filter_parameter = 0.925, passes = 3) # m^3/day

# Step 3: Plot streamflow and baseflow separation
#   Hint use plot(), lines(), and legend()
plot(as.Date(WSR$flowdata$date), flow_CMS, type = "l", xlab = "Date", ylab = "Flow in m^3/s")
lines(as.Date(WSR$flowdata$date), bfs[,1], type = "l", col = "red")

# Step 4: Calculate the total baseflow and streamflow by year of the dataset. Plot the results.
#   Hint: Use brackets and a colon (i.e. [1:365]) to select a specfic range from the flow dataset.
#   ?barplot
bfs$Year = as.numeric(format(as.Date(WSR$flowdata$date),"%Y")) #this makes a new column for just year pulled out of the flowdata frame
bfs$TotalF = bfs$bt + bfs$qft
bf_annual = matrix(ncol = 3, nrow = 2017-1990)

for (year in 1990:2016)
{
  
  bf_annual[year - 1989,1] = year # to populate years
  bf_annual[year - 1989,2] = sum(bfs[bfs$Year == year,1]) # to get baseflow contribution
  bf_annual[year - 1989,3] = sum(bfs[bfs$Year == year,4]) # to get total streamflow
}

colnames(bf_annual) = c("Year","BF","Total Streamflow")
bf_sf_matrix = t(bf_annual[,-1]) # makes a matrix of the data we care about (baseflow and streamflow), then transposes it so we can bar plot it
colnames(bf_sf_matrix) = bf_annual[,1] # makes the x-axis labels
barplot(bf_sf_matrix,horiz=FALSE,legend = c("baseflow","runoff")) #

# Flashiness Index --------------------------------------------------------
# needs to be computed at a daily timestep

# Refer to Baker et al., 2004 (doi:10.1111/j.1752-1688.2004.tb01046.x) and
# calculate the flashiness index with your dataset.

Flash = data.frame(matrix(nrow = length(bfs$qft), ncol = 0))
i=2
for (i in 1:length(bfs$TotalF))
{
  Flash$Numerator[i] = abs((bfs[i,4])-(bfs[i-1,4]))
  Flash$Denominator[i] = bfs[i,4]
}

# Get rid of first row that has a NaN
Flash <- Flash[-1,]

# Sum all of the values in the numerator and denominator column to calculate the total flashiness index
FlashNumeratorSum = sum(Flash$Numerator)
FlashDenominatorSum = sum(Flash$Denominator)
RB = FlashNumeratorSum/FlashDenominatorSum #total flashiness index

