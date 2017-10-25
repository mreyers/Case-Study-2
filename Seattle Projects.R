# Start of the Case Study 2 Project
# Currently being done with the Seattle Emergency Data, calling from API

# Libraries
library(RCurl)
library(jsonlite)
library(Rfast)
library(ggmap)

# Function 1: Cleaner. Needed because data will likely be subset, no need to repeat code
CleanerSeattle = function(df){
  # Recode Event Number and Offense Number to factor
  df$CAD.Event.Number = as.factor(df$CAD.Event.Number)
  df$General.Offense.Number = as.factor(df$General.Offense.Number)
  
  # Convert date time character variables into actual date-times
  df$Event.Clearance.Date = as.POSIXct(strptime(df$Event.Clearance.Date, format = "%m/%d/%Y %H:%M:%S"))
  df$At.Scene.Time        = as.POSIXct(strptime(df$At.Scene.Time, format = "%m/%d/%Y %H:%M:%S"))
  
  return(df)
}


Seattle = read.csv("~/GitHub/Case-Study-2/Seattle_Police_Department_911_Incident_Response.csv", as.is = TRUE, strip.white = TRUE)
Cincinnati = read.csv("~/GitHub/Case-Study-2/PDI_Police_Calls_For_Service__CAD_.csv", as.is = TRUE, strip.white = TRUE)

Seattle = CleanerSeattle(Seattle)

clean = CleanerSeattle(Seattle)
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

  # Sort out data with longitude and latitude, make sure to return two data frames as a list
  subsetRows = !is.na(df$LONGITUDE_X) & !is.na(df$LATITUDE_X)
  address = df[ subsetRows,]
  noAddress=df[!subsetRows,]
  
  return(list(address, noAddress))
}
# Add any further cleaning operations to the generic cleaner above for consistency

# Exploring Cincinnati1
Cincinnati1 = CleanerCin(Cincinnati1)
CinAddress = Cincinnati1[[1]]
CinNoAdd   = Cincinnati1[[2]]

# Google maps image for the data with addresses
map = get_googlemap('cincinnati', scale = 2, zoom = 14)
CinMap = ggmap(map, extent = "device", legend = "none")
CinMapWithCrimePoints = CinMap + geom_point(data = CinAddress, aes(x = LONGITUDE_X, y = LATITUDE_X))
CinMapWithCrimePoints
CinMapWithCrimeLevels = CinMap + stat_density2d(aes(x = LONGITUDE_X, y = LATITUDE_X, fill = ..level.., alpha = ..level..),
size = 2, bins = 4, geom = "polygon", data = CinAddress)
CinMapWithCrimeLevels

head(CinAddress$INCIDENT_TYPE_ID)
x = 1










################################ SEATTLE BORDER ##########################################
# Subsets to work with if doing Seattle stuff
uniq = unique(Seattle$District.Sector)
uniq = sort(uniq, decreasing = TRUE)
for(i in 1:length(uniq)){
  assign(paste("Seattle", uniq[i], sep =""), Seattle[Seattle$District.Sector == uniq[i],])
}
names(SeattleM)
# Seattle map, use to check the area codes I have
mapSeattle = get_googlemap('Seattle', scale = 2, zoom = 11)
SeattleMap = ggmap(mapSeattle, extent = "device", legend = "none")
SeattleMapWithCrime = SeattleMap + geom_point(data = SeattleM, aes(x = Longitude, y = Latitude))
SeattleMapWithCrime
SeattleMapWithCrimeLevels = SeattleMap +stat_density2d(aes(x = Longitude, y = Latitude, fill = ..level.., alpha = ..level..),
                                                       size = 2, bins = 4, geom = "polygon", data = SeattleM)
