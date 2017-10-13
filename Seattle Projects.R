# Start of the Case Study 2 Project
# Currently being done with the Seattle Emergency Data, calling from API

# Libraries
library(RCurl)
library(jsonlite)
library(Rfast)

# Function 1: Cleaner. Needed because data will likely be subset, no need to repeat code
Cleaner = function(df){
  # Recode Event Number and Offense Number to factor
  df$CAD.Event.Number = as.factor(df$CAD.Event.Number)
  df$General.Offense.Number = as.factor(df$General.Offense.Number)
  
  # Convert date time character variables into actual date-times
  df$Event.Clearance.Date = as.POSIXct(strptime(df$Event.Clearance.Date))
  df$At.Scene.Time        = as.POSIXct(strptime(df$At.Scene.Time))
  
}

range(Seattle$At.Scene.Time)

Seattle = read.csv("Seattle_Police_Department_911_Incident_Response.csv", as.is = TRUE, strip.white = TRUE)

# These are the subsets so that the data can be stored in github, had a 100MB limit
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
