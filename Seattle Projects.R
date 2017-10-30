# Start of the Case Study 2 Project
# Currently being done with the Seattle Emergency Data, calling from API

#Install Packages
install.packages("lubridate")

# Libraries
library(RCurl)
library(jsonlite)
library(Rfast)
library(ggmap)
library(ggplot2)
library(stringi)
library(lubridate)
library(tidyverse)

Cincinnati = read.csv("~/GitHub/Case-Study-2/PDI_Police_Calls_For_Service__CAD_.csv", as.is = TRUE, strip.white = TRUE)
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

# Function 1: Cleaner. Needed because data will likely be subset, no need to repeat code
CleanerSeattle = function(df){
  # Recode Event Number and Offense Number to factor
  df$CAD.Event.Number = as.factor(df$CAD.Event.Number)
  df$General.Offense.Number = as.factor(df$General.Offense.Number)
  
  # Convert date time character variables into actual date-times
  df$Event.Clearance.Date = as.POSIXct(strptime(df$Event.Clearance.Date, format = "%m/%d/%Y %H:%M:%S"))
  df$At.Scene.Time = as.POSIXct(strptime(df$At.Scene.Time, format = "%m/%d/%Y %H:%M:%S"))
  
  return(df)
}


Seattle = read.csv("~/GitHub/Case-Study-2/Seattle_Police_Department_911_Incident_Response.csv", as.is = TRUE, strip.white = TRUE)
Seattle = CleanerSeattle(Seattle)

# Subsets for each District Sector existing in our data
uniq = unique(Seattle$District.Sector)
uniq = sort(uniq, decreasing = TRUE)
SeattleWhole = Seattle
for(i in 1:length(uniq)){
  assign(paste("Seattle", uniq[i], sep =""), Seattle[Seattle$District.Sector == uniq[i],])
}


# Seattle map, use to check the area codes I have
mapSeattle = get_googlemap('Seattle', scale = 2, zoom = 11)
SeattleMap = ggmap(mapSeattle, extent = "device", legend = "none")
# Basic plots showing that there are differences in zone usage 
SeattleMapWithCrimeM = SeattleMap + geom_point(data = SeattleM, aes(x = Longitude, y = Latitude))
SeattleMapWithCrimeM
SeattleMapWithCrimeB = SeattleMap + geom_point(data = SeattleB, aes(x = Longitude, y = Latitude))
SeattleMapWithCrimeB

# Next step, plots with the severity of crime or something along those lines
SeattleMapWithCrimeLevels = SeattleMap +stat_density2d(aes(x = Longitude, y = Latitude, fill = ..level.., alpha = ..level..),
                                                       size = 2, bins = 4, geom = "polygon", data = SeattleM)






# Number of Calls
# Working with a random sample of the Seattle data to begin with.

fraction = 0.05 # Going to work with 5% of the data
set.seed(123)
Seattle2 = Seattle
Seattle2$Event.Clearance.Date = as.POSIXct(strptime(Seattle2$Event.Clearance.Date, format = "%m/%d/%Y %I:%M:%S %p"))

# Split Data
# Seattle used here is the full data.  Not the one from splitting the data into districts.
sample_rows = sample(1:nrow(Seattle), floor(nrow(Seattle) * fraction))
dat_sample = Seattle[sample_rows,]
write.csv(dat_sample,"Seattle_Fraction_05.csv")

dim(dat_sample)
str(dat_sample$Event.Clearance.Date)
dat_sample$Event.Clearance.Date = as.POSIXct(strptime(dat_sample$Event.Clearance.Date, format = "%m/%d/%Y %I:%M:%S %p"))
dat_sample$Event.Clearance.Date # Now in 24 hour clock
str(times)


callsPerDay <- function(data, fullDF){
  # Returns one of two things.
  # If fullDF = TRUE then returns data with calls two new columns (rounded days, and calls that day)
  # If fullDF = FALSE then returns the rounded days and number of calls for that day.
  # Round Incidents to the day. Removes hour, minutes, seconds, and am/pm (If not in 24hour Clock)
  if(fullDF == TRUE){
    #Convert events to time that is useable.
    data$Event.Clearance.Date = as.POSIXct(strptime(data$Event.Clearance.Date, format = "%m/%d/%Y %I:%M:%S %p"))
    timesDay <- as.character(round(data$Event.Clearance.Date, "day"))
    data <- cbind(data, timesDay) # Add it to the dataframe
    
    data2 = data # Copy for later.
    
    # Obtain count of calls per day
    data2 %>%
      group_by(timesDay) %>%
      summarise(timesPerDay = n()) %>%
      mutate(timesPerDay) -> data2
    
    # Combine data and count.  Then return.
    data <- merge(data, data2, by = "timesDay")
    return(data)
    
  }else{ # If fullDF = FALSE
    
    #Convert events to time that is useable.
    data$Event.Clearance.Date = as.POSIXct(strptime(data$Event.Clearance.Date, format = "%m/%d/%Y %I:%M:%S %p"))
    timesDay <- as.character(round(data$Event.Clearance.Date, "day"))
    data <- cbind(data, timesDay) # Add it to the dataframe
    
    # Obtain count of calls per day
    data %>%
      group_by(timesDay) %>%
      summarise(timesPerDay = n()) %>%
      mutate(timesPerDay) -> data
    return(data)
  }
}

Seattle3 <- callsPerDay(Seattle, FALSE)
View(Seattle3)

callsPerWeek <- function(data, fullDF){
  # Returns one of two things.
  # If fullDF = TRUE then returns data with calls two new columns (rounded weeks, and calls that week)
  # If fullDF = FALSE then returns the rounded weeks and number of calls for that week.
  # Round Incidents to the week. Removes days, hours, minutes, seconds, and am/pm
  if(fullDF == TRUE){
    #Convert events to time that is useable.
    data$Event.Clearance.Date = as.POSIXct(strptime(data$Event.Clearance.Date, format = "%m/%d/%Y %I:%M:%S %p"))
    timesWeek <- as.character(round(data$Event.Clearance.Date, "week"))
    data <- cbind(data, timesWeek) # Add it to the dataframe
    
    data2 = data # Copy for later.
    
    # Obtain count of calls per week
    data2 %>%
      group_by(timesWeek) %>%
      summarise(timesPerWeek = n()) %>%
      mutate(timesPerWeek) -> data2
    
    # Combine data and count.  Then return.
    data <- merge(data, data2, by = "timesWeek")
    return(data)
    
  }else{ # If fullDF = FALSE
    
    #Convert events to time that is useable.
    data$Event.Clearance.Date = as.POSIXct(strptime(data$Event.Clearance.Date, format = "%m/%d/%Y %I:%M:%S %p"))
    timesWeek <- as.character(round(data$Event.Clearance.Date, "week"))
    data <- cbind(data, timesWeek) # Add it to the dataframe
    
    # Obtain count of calls per week
    data %>%
      group_by(timesWeek) %>%
      summarise(timesPerWeek = n()) %>%
      mutate(timesPerWeek) -> data
    return(data)
  }
}


# Event Variables
unique(Seattle$Event.Clearance.Description)
unique(Seattle$Event.Clearance.SubGroup)
unique(Seattle$Event.Clearance.Group)

ClearanceGroups = xtabs(~Seattle$Event.Clearance.Group + Seattle$Event.Clearance.Group)
sort(ClearanceGroups)
View(sort(ClearanceGroups))


Seattle.Events.Grouped <- group_by(Seattle, Event.Clearance.Group, Event.Clearance.SubGroup, Event.Clearance.Description)
View(Seattle.Events.Grouped)

View(SeattleGB_Clearance.Group)

eventCategorizer_Violence <- function(dataframe){
  # Categorizes each event as Violent or Non-Violent
  # First Round of classifying.  Checks if Group can classify right away.
  # Example: Homicide is violent. 
  
  # Remove useless or non call events
  
  dataframe <- dataframe[dataframe$Event.Clearance.Group != "", ] # Remove the rows that were not calls
  dataframe <- dataframe[dataframe$Event.Clearance.Group != "NULL", ] # Remove the rows that had not useable information.
  
  
  # Split out the columns we need.
  # Event.Clearance.Description
  # Event.Clearance.SubGroup
  # Event.Clearance.Group
  df <- select(dataframe, starts_with("Event.Clearance."))
  
  # Events that were the result of violent encounters or have a strong (subjective) chance of being.
  violent.Events <- c("HOMICIDE",
                      "DRIVE BY (NO INJURY)",
                      "WEAPONS CALLS", "ROBBERY",
                      "THREATS, HARASSMENT",
                      "ARREST", # Might need to change these
                      "ASSAULTS")

  
  # Events that are non violent or vast majority of encounters would likely not be violent.
  NonViolent.Events <- c("LIQUOR VIOLATIONS",
                         "RECKLESS BURNING",
                         "PUBLIC GATHERINGS",
                         "FAILURE TO REGISTER (SEX OFFENDER)",
                         "HARBOR CALLS",
                         "PROWLER",
                         "LEWD CONDUCT",
                         "PROSTITUTION",
                         "ANIMAL COMPLAINTS",
                         "PERSONS - LOST, FOUND, MISSING",
                         "MISCELLANEOUS MISDEMEANORS",
                         "NUISANCE, MISCHIEF",
                         "PROPERTY - MISSING, FOUND",
                         "FRAUD CALLS",
                         "FALSE ALACAD",
                         "PROPERTY DAMAGE",
                         "NARCOTICS COMPLAINTS",
                         "NUISANCE, MISCHIEF",
                         "AUTO THEFTS",
                         "FALSE ALARMS",
                         "SHOPLIFTING",
                         "ACCIDENT INVESTIGATION",
                         "OTHER PROPERTY",
                         "MOTOR VEHICLE COLLISION INVESTIGATION",
                         "BURGLARY",
                         "TRESPASS",
                         "CAR PROWL",
                         "DISTURBANCES",
                         "SUSPICIOUS CIRCUMSTANCES",
                         "BEHAVIOURAL HEALTH", 
                         "BIKE", 
                         "VICE CALLS", 
                         "OTHER VICE", 
                         "MENTAL HEALTH", 
                         "HAZARD", 
                         "TRAFFIC RELATED CALLS")
  
  # Calls that I wasn't sure about. Could be either.  Simplifies everything if we just put in one or the other.
  # NULL calls have no information.  Just remove.
  # "" calls probably weren't calls but when the police officer stumbled upon 
  # Arrests are kinda ambigious?
  # Will put them in violent for now.
  ambiguous.Events <- c("ARREST",
                        "")
  
  violentsCat <- rep(NA, nrow(df))
  
  for(i in 1:nrow(df)){ # For each row in the dataframe
    for(j in 1:length(violent.Events)) # Cycle through each violent event
    if(df$Event.Clearance.Group[i] == violent.Events[j]){ # See if any match
      violentsCat[i] = "Violent" # If any do, call them Violent
    }else{ 
      violentsCat[i] = "Non Violent"
        }
  }
  return(violentsCat)
}


# Testing out event Categorizer_Violence
View(dat_sample)
violentEventzzz <- eventCategorizer_Violence(dat_sample)
violentEventzzz

# Works ish

# Plots and Counts
dat_sample2 <- callsPerDay(dat_sample, TRUE)
View(dat_sample2)

NewSeattle <- callsPerDay(Seattle)
View(NewSeattle)

NewSeattle %>%
  group_by(timesDay) %>%
  summarise(callsThatDay = n()) %>%
  ggplot(aes(x = timesDay, y = callsThatDay)) + geom_point() + ylim(0,1200)


timeSeq <- seq(as.Date("2010-07-17"), as.Date("2017-09-08"), by = "days")


dayPlot_Count <- plot_CallCount(Seattle, timeSeq)  
dayPlot_Count



# ELO SECTION

# ELO Function Basic Setup
eloFunction = function(ARating, BRating, LearnCoef, Result){
  
  # Expectations
  EA = (1 / (1 + 10^((BRating - ARating)/400)))
  EB = (1 / (1 + 10^((ARating - BRating)/400)))
  
  # Update Ratings Based on Outcome
  # A updated
  newRatingAWin = ARating + LearnCoef * (1 - EA)
  newRatingADraw= ARating + LearnCoef * (0.5-EA)
  newRatingALoss= ARating + LearnCoef * (0 - EA)
  
  # B updated
  newRatingBWin = BRating + LearnCoef * (1 - EB)
  newRatingBDraw= BRating + LearnCoef * (0.5-EB)
  newRatingBLoss= BRating + LearnCoef * (0 - EB)
  
  # Return updated rankings based on Result
  if (Result > 0){
    # Win for A
    df = cbind(newRatingAWin, newRatingBLoss)
  }
  else if (Result < 0){
    # Win for B
    df = cbind(newRatingALoss, newRatingBWin)
  }
  else{
    df = cbind(newRatingADraw, newRatingBDraw)
  }
  
  colnames(df) = c("playerA", "playerB")
  return(df)
}

# How to use the ELO Function:
  # ARating, BRating, LearnCoef are all basic components of ELO
    # ARating: ELO of A before 'match'
    # BRating: ELO of B before 'match'
    # LearnCoef: Rate of learning, decays over time
  # Result is the outcome of the match
    # Result > 0 is a win for PLAYER A
    # Result < 0 is a win for PLAYER B
    # Result = 0 is a draw

# Testing ELO function
ResultELO = eloFunction(1200, 1000, 20, 0)
ResultELO


# Game Function
# Decide whether or not the "player" won their match

gameOutcome <- function(playerData, oppData){
  # Based on specified number of crimes in certain categories, we decided if that district won or lost the game
  # Plays against the average of the other districts
  
}


