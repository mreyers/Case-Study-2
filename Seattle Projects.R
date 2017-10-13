# Start of the Case Study 2 Project
# Currently being done with the Seattle Emergency Data, calling from API

# Libraries
library(RCurl)
library(jsonlite)
library(Rfast)

# Function 1: Cleaner. Needed because data will likely be subset, no need to repeat code
CleanerSeattle = function(df){
  # Recode Event Number and Offense Number to factor
  df$CAD.Event.Number = as.factor(df$CAD.Event.Number)
  df$General.Offense.Number = as.factor(df$General.Offense.Number)
  
  # Convert date time character variables into actual date-times
  df$Event.Clearance.Date = as.POSIXct(strptime(df$Event.Clearance.Date))
  df$At.Scene.Time        = as.POSIXct(strptime(df$At.Scene.Time))
  
}

range(Seattle$At.Scene.Time)

Seattle = read.csv("~/GitHub/Case-Study-2/Seattle_Police_Department_911_Incident_Response.csv", as.is = TRUE, strip.white = TRUE)
Cincinnati = read.csv("~/GitHub/Case-Study-2/PDI_Police_Calls_For_Service__CAD_.csv", as.is = TRUE, strip.white = TRUE)

names(Cincinnati)

View(head(Cincinnati))
dim(Cincinnati)

# Subsets to work with if doing Cincinnati stuff
Cincinnati1 = Cincinnati[1:(0.2*length(Cincinnati$ADDRESS_X)),]
Cincinnati2 = Cincinnati[(0.2*length(Cincinnati$ADDRESS_X)): (0.4*length(Cincinnati$ADDRESS_X)),]
Cincinnati3 = Cincinnati[(0.4*length(Cincinnati$ADDRESS_X)): (0.6*length(Cincinnati$ADDRESS_X)),]
Cincinnati4 = Cincinnati[(0.6*length(Cincinnati$ADDRESS_X)): (0.8*length(Cincinnati$ADDRESS_X)),]
Cincinnati5 = Cincinnati[(0.8*length(Cincinnati$ADDRESS_X)): (    length(Cincinnati$ADDRESS_X)),]

# Check types of the variables in reduced data sets and what we have
str(Cincinnati1)

# Create a Cincinnati cleaner
CleanerCin = function(df){
  # Time conversions
  df$CREATE_TIME_INCIDENT = as.POSIXct(strptime(df$CREATE_TIME_INCIDENT, format = "%m/%d/%Y %H:%M:%S"))
  df$ARRIVAL_TIME_PRIMARY_UNIT = as.POSIXct(strptime(df$ARRIVAL_TIME_PRIMARY_UNIT, format = "%m/%d/%Y %H:%M:%S"))
  df$CLOSED_TIME_INCIDENT = as.POSIXct(strptime(df$CLOSED_TIME_INCIDENT, format = "%m/%d/%Y %H:%M:%S"))
  df$DISPATCH_TIME_PRIMARY_UNIT = as.POSIXct(strptime(df$DISPATCH_TIME_PRIMARY_UNIT, format = "%m/%d/%Y %H:%M:%S"))
  return(df)
}
# Add any further cleaning operations to the generic cleaner above for consistency

# Exploring Cincinnati1
Cincinnati1 = CleanerCin(Cincinnati1)










################################ SEATTLE BORDER ##########################################
# Subsets to work with if doing Seattle stuff
# Seattle1 = Seattle[1:(0.2*length(Seattle$CAD.CDW.ID)),]
# Seattle2 = Seattle[(0.2*length(Seattle$CAD.CDW.ID)+1): (0.4*length(Seattle$CAD.CDW.ID)),]
# Seattle3 = Seattle[(0.4*length(Seattle$CAD.CDW.ID)+1): (0.6*length(Seattle$CAD.CDW.ID)),]
# Seattle4 = Seattle[(0.6*length(Seattle$CAD.CDW.ID)+1): (0.8*length(Seattle$CAD.CDW.ID)),]
# Seattle5 = Seattle[(0.8*length(Seattle$CAD.CDW.ID)+1): (    length(Seattle$CAD.CDW.ID)),]
# 
# dim(Seattle4)
# write.csv(Seattle1, file = "Seattle1.csv")
# write.csv(Seattle2, file = "Seattle2.csv")
# write.csv(Seattle3, file = "Seattle3.csv")
# write.csv(Seattle4, file = "Seattle4.csv")
# write.csv(Seattle5, file = "Seattle5.csv")


dim(Seattle5)
dim(Seattle2)
dim(Seattle1)
head(Seattle1)
head(Seattle)

# Check data structure
str(Seattle)

# Check the number of typos in clearance description
item = table(Seattle$Event.Clearance.Description)
item[item < 20]
item2 = table(Seattle$Event.Clearance.SubGroup)
item2[item2<2000]
# Doesn't appear to be any typos
# Data possibly collected via drop down menus, meaning clean subgroups
