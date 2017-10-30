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

gameOutcome <- function(playerData, oppData){
  # Based on specified number of crimes in certain categories, we decided if that district won or lost the game
  # Plays against the average of the other districts
  
}

