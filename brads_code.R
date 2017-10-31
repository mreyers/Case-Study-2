# Brad's Code

# Packages
install.packages("forecast")
install.packages("ggplot2")
install.packages("TSA")
install.packages("scales")
install.packages("rnoaa")

# Library
library(dplyr)
library(forecast)
library(tseries)
library(ggplot2)
library(dplyr)
library(TSA)
library(scales)
library(rnoaa)
library(stringr)


setwd("C:/Users/Brad_/Desktop/SFU/Statistics/Statistics 440/Case Studies/Case Study 2")
Seattle <- read.csv()

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


callsPerHour <- function(data, fullDF){
  # Returns one of two things.
  # If fullDF = TRUE then returns data with calls two new columns (rounded hours, and calls that hour)
  # If fullDF = FALSE then returns the rounded hour and number of calls for that hour.
  # Round Incidents to the hour. Removes minutes, seconds, and am/pm
  if(fullDF == TRUE){
    #Convert events to time that is useable.
    data$Event.Clearance.Date = as.POSIXct(strptime(data$Event.Clearance.Date, format = "%m/%d/%Y %I:%M:%S %p"))
    timesHour <- substr(data$Event.Clearance.Date, 1,13)
    data <- cbind(data, timesHour) # Add it to the dataframe
    
    data2 = data # Copy for later.
    
    # Obtain count of calls per week
    data2 %>%
      group_by(timesHour) %>%
      summarise(timesPerHour = n()) %>%
      mutate(timesPerHour) -> data2
    
    # Combine data and count.  Then return.
    data <- merge(data, data2, by = "timesHour")
    return(data)
    
  }else{ # If fullDF = FALSE
    
    #Convert events to time that is useable.
    data$Event.Clearance.Date = as.POSIXct(strptime(data$Event.Clearance.Date, format = "%m/%d/%Y %I:%M:%S %p"))
    timesHour <- substr(data$Event.Clearance.Date, 1,13)
    data <- cbind(data, timesHour) # Add it to the dataframe
    
    # Obtain count of calls per week
    data %>%
      group_by(timesHour) %>%
      summarise(timesPerHour = n()) %>%
      mutate(timesPerHour) -> data
    return(data)
  }
}



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


###########################################################################################################
########################################### SEATTLE Time Series Models ####################################
###########################################################################################################

Seattle <- read.csv("Seattle_Police_Department_911_Incident_Response.csv", header = TRUE)
View(Seattle)

SeattleData <- callsPerDay(Seattle, FALSE) # Round off the number of calls to the day
SeattleData2 <- SeattleData[1271:2289, ] # Row 1271 is the start of 2015

View(SeattleData)
View(SeattleData2)

SeattleNumbers <- SeattleData2[,2]
SeattleNumbers


ts.Seattle <- ts(SeattleNumbers, start = 1, end = 1019)

# Split Seattle Time Series Data into Test and Train


dim(ts.Seattle)

train.Seattle <- ts.Seattle[1:(0.75*nrow(ts.Seattle)), ]
test.Seattle <- ts.Seattle[(0.75*nrow(ts.Seattle)+ 1):nrow(ts.Seattle), ]

train.Seattle # 764 Observations
test.Seattle # 254 observations

Seattle.fit2 <- auto.arima(train.Seattle)
Seattle.fit2

Seattle.sim <- arima.sim(n = 253, list(order = c(1,1,3), ar = c(-0.2790), ma = c(-0.1779, -0.4842, -0.2253)))
Seattle.sim

plot(test.Seattle, type = "l")
plot(Seattle.sim)

View(ts.Seattle)
plot.ts(ts.Seattle)

acf(ts.Seattle)
pacf(ts.Seattle)

seattle.fit <- auto.arima(ts.Seattle)
seattle.fit

seattle.pred2 <- predict(seattle.fit, n.ahead = 30)
seattle.pred <- forecast(seattle.fit, h = 30)
seattle.pred
str(seattle.pred)

plot(seattle.pred)
plot.ts(seattle.pred)


# Adding Weather to the model

station_data = ghcnd_stations()
Seattle_station=filter(station_data, latitude<48, latitude>47,
                       state=="WA", last_year==2017)
View(Seattle_station)

weather_merp=ghcnd_search("USW00024233", var = c("TAVG", "PRCP"), date_min = "2015-01-01", date_max = "2017-10-22")
#weather_merp=ghcnd_search("USC00458278", var = "all", date_min = "2010-07-18", date_max = "2017-10-22")


weather_merp
View(weather_merp)
str(weather_merp)

tempData <- as.data.frame(weather_merp[1])
tempData

avgTemp <- tempData[1:1019, 2]
avgTemp

prcpData <- as.data.frame(weather_merp[2])
prcpData

amountRain <- prcpData[1:1019, 2]
amountRain

plot(amountRain, type = "l")

acf(amountRain)

rain.fit <- auto.arima(amountRain)
rain.fit

totalWeatherData = cbind(amountRain, avgTemp)

# Multivariate Time Series Modelling 

multi1.fit <- auto.arima(ts.Seattle, xreg = amountRain) # Rain data added
multi1.fit

multi2.fit <- auto.arima(ts.Seattle, xreg = avgTemp) # Temperature Data added
multi2.fit

multi3.fit <- auto.arima(ts.Seattle, xreg = totalWeatherData) # Rain and Temperature Data added
multi3.fit

multi3.pred <- forecast(multi3.fit, h = 30)

# Two model comparison

multi1.fit # With Rain
multi2.fit # With Temp
multi3.fit # With Rain and Temp (FULL MODEL)
seattle.fit # Plain ARIMA model

# BIC says they are all basically the same
seattle.fit$bic
multi1.fit$bic
multi2.fit$bic
multi3.fit$bic


#### Crimes Rounded to the Hour ####

SeattleHours <- callsPerHour(Seattle, FALSE)
View(SeattleHours) # 26908

SeattleHours2 <- SeattleHours[26908:nrow(SeattleHours), ]
SeattleHours2

SeattleHours.Num <- ts(SeattleHours2[,2])

SeattleHours.fit <- auto.arima(SeattleHours.Num)
SeattleHours.fit




###########################################################################################################

#### Baltimore Analysis #####

# Data
balt <- read.csv("baltimore_cleaned_hp.csv", header = TRUE)
balt
View(balt)
names(balt)

# 
?arima()

Weeks <- c(1:150)

balt %>%
  dplyr::filter(District == "CD") %>%
  select(number) -> balt_CD

balt_CD <- ts(balt_CD, start = 1, end = 150)
balt_CD

dist_CD
View(dist_CD)
str(dist_CD)
str(dist_CD$number)


?ts()

plot.ts(balt_CD, type = "l")

ggplot(dist_CD, aes(x = Weeks, y = number)) + geom_line()

?acf()
# Check stationarity with acf
acf(balt_CD, lag.max = 30)
# Does not appear stationary.  Too many significant lags.  
# After differencing it looks much better.  Looks pretty stationary

# Difference time series
balt_CD <- diff(balt_CD)
balt_CD

pacf(balt_CD)

eacf(balt_CD)

?auto.arima
fit.CD1 <- auto.arima(balt_CD)
# Recommends a MA(1) model.  One difference (I did on my own) and no AR component.
# Integrated Moving Average

fit.CD1
str(fit.CD1)

?arima()
fit.CD2 <- arima(balt_CD, order = c(0,0,1))
fit.CD

Districts = unique(balt$District)
Districts[1]

timeSeries_Balt <- function(dataframe){
  
  Districts = unique(dataframe$District) # Find all districts from dataframe
  models <- list() # Initialize vector that will contain data from each model
  
  for(i in 1:length(Districts)){ # Fitting an arima model for each district in the data
    dataframe %>%
      dplyr::filter(District == Districts[i]) %>%
      dplyr::select(number) -> District_
    
    District_ <- ts(District_, start = 1, end = 150)
    models[[i]] <- auto.arima(District_)
  }
  return(models)
}

Balt.Dist.Models <- timeSeries_Balt(balt)
Balt.Dist.Models

### All the Baltimore Districts Manually

Districts

### District 1: CD

# Data
balt %>%
  dplyr::filter(District == "CD") %>%
  select(number) -> balt_CD


# Plots
plot(balt_CD)
acf(balt_CD)

balt_CD <- ts(balt_CD, start = 1, end = 150)
fit.CD <- auto.arima(balt_CD)
fit.CD
pred.CD <- predict(fit.CD, n.ahead = 1)
pred.CD <- forecast(fit.CD, h = 5)
plot(pred.CD)

### District 2: CW

# Data
balt %>%
  dplyr::filter(District == "CW") %>%
  select(number) -> balt_CW

balt_CW <- ts(balt_CW, start = 1, end = 150)

# Plots
plot(balt_CW)

# Model
fit.CW <- auto.arima(balt_CW)
fit.CW

# Forecast
pred.CW <- predict(fit.CW, n.ahead = 5)
pred.CW <- forecast(fit.CW, h = 5)
pred.CW
plot(pred.CW)


pred2.CW <-funggcast(balt_CW, pred.CW)

### District 3: ED

# Data
balt %>%
  dplyr::filter(District == "ED") %>%
  select(number) -> balt_ED

balt_ED <- ts(balt_ED, start = 1, end = 150)

# Plots
plot(balt_ED)

# Model
fit.ED <- auto.arima(balt_ED)
fit.ED

# Forecast
pred.ED <- predict(fit.ED, n.ahead = 5)
pred.ED <- forecast(fit.ED, h = 5)
pred.ED
plot(pred.ED)

forecast_CW<-ggplot(data=pred2.CW,aes(x=Point,y=Forecast)) 
p1a<-p1a+geom_line(col='red')
p1a<-p1a+geom_line(aes(y=fitted),col='blue')

### District 4: EVT1

# Data
balt %>%
  dplyr::filter(District == "EVT1") %>%
  select(number) -> balt_EVT1

balt_EVT1 <- ts(balt_EVT1, start = 1, end = 150)

# Plots
plot(balt_EVT1)

# Model
fit.EVT1 <- auto.arima(balt_EVT1)
fit.EVT1

# Forecast
pred.EVT1 <- predict(fit.EVT1, n.ahead = 5)
pred.EVT1 <- forecast(fit.EVT1, h = 5)
pred.EVT1
plot(pred.EVT1)


### District 5: EVT2

# Data
balt %>%
  dplyr::filter(District == "EVT2") %>%
  select(number) -> balt_EVT2

balt_EVT2 <- ts(balt_EVT2, start = 1, end = 150)

# Plots
plot(balt_EVT2)

# Model
fit.EVT2 <- auto.arima(balt_EVT2)
fit.EVT2

# Forecast
pred.EVT2 <- predict(fit.EVT2, n.ahead = 5)
pred.EVT2 <- forecast(fit.EVT2, h = 5)
pred.EVT2
plot(pred.EVT2)


### District 6: HP

# Data
balt %>%
  dplyr::filter(District == "HP") %>%
  select(number) -> balt_HP

balt_HP <- ts(balt_HP, start = 1, end = 150)

# Plots
plot(balt_HP)

# Model
fit.HP <- auto.arima(balt_HP)
fit.HP

# Forecast
pred.HP <- predict(fit.HP, n.ahead = 5)
pred.HP <- forecast(fit.HP, h = 5)
pred.HP
plot(pred.HP)


### District 7: ND

# Data
balt %>%
  dplyr::filter(District == "ND") %>%
  select(number) -> balt_ND

balt_ND <- ts(balt_ND, start = 1, end = 150)

# Plots
plot(balt_ND)

# Model
fit.ND <- auto.arima(balt_ND)
fit.ND

# Forecast
pred.ND <- predict(fit.ND, n.ahead = 5)
pred.ND <- forecast(fit.ND, h = 5)
pred.ND
plot(pred.ND)


### District 8: NE

# Data
balt %>%
  dplyr::filter(District == "NE") %>%
  select(number) -> balt_NE

balt_NE <- ts(balt_NE, start = 1, end = 150)

# Plots
plot(balt_NE)

# Model
fit.NE <- auto.arima(balt_NE)
fit.NE

# Forecast
pred.NE <- predict(fit.NE, n.ahead = 5)
pred.NE <- forecast(fit.NE, h = 5)
pred.NE
plot(pred.NE)


### District 9: NW

# Data
balt %>%
  dplyr::filter(District == "NW") %>%
  select(number) -> balt_NW

balt_NW <- ts(balt_NW, start = 1, end = 150)

# Plots
plot(balt_NW)

# Model
fit.NW <- auto.arima(balt_NW)
fit.NW

# Forecast
pred.NW <- predict(fit.NW, n.ahead = 5)
pred.NW <- forecast(fit.NW, h = 5)
pred.NW
plot(pred.NW)


### District 10: SD

# Data
balt %>%
  dplyr::filter(District == "SD") %>%
  select(number) -> balt_SD

balt_SD <- ts(balt_SD, start = 1, end = 150)

# Plots
plot(balt_SD)

# Model
fit.SD <- auto.arima(balt_SD)
fit.SD

# Forecast
pred.SD <- predict(fit.SD, n.ahead = 5)
pred.SD <- forecast(fit.SD, h = 5)
pred.SD
plot(pred.SD)


### District 11: SE

# Data
balt %>%
  dplyr::filter(District == "SE") %>%
  select(number) -> balt_SE

balt_SE <- ts(balt_SE, start = 1, end = 150)

# Plots
plot(balt_SE)

# Model
fit.SE <- auto.arima(balt_SE)
fit.SE

# Forecast
pred.SE <- predict(fit.SE, n.ahead = 5)
pred.SE <- forecast(fit.SE, h = 5)
pred.SE
plot(pred.SE)


### District 12: SS

# Data
balt %>%
  dplyr::filter(District == "SS") %>%
  select(number) -> balt_SS

balt_SS <- ts(balt_SS, start = 1, end = 150)

# Plots
plot(balt_SS)

# Model
fit.SS <- auto.arima(balt_SS)
fit.SS

# Forecast
pred.SS <- predict(fit.SS, n.ahead = 5)
pred.SS <- forecast(fit.SS, h = 5)
pred.SS
plot(pred.SS)


### District 13: SW

# Data
balt %>%
  dplyr::filter(District == "SW") %>%
  select(number) -> balt_SW

balt_SW <- ts(balt_SW, start = 1, end = 150)

# Plots
plot(balt_SW)

# Model
fit.SW <- auto.arima(balt_SW)
fit.SW

# Forecast
pred.SW <- predict(fit.SW, n.ahead = 5)
pred.SW <- forecast(fit.SW, h = 5)
pred.SW
plot(pred.SW)


### District 14: TRU

# Data
balt %>%
  dplyr::filter(District == "TRU") %>%
  select(number) -> balt_TRU

balt_TRU <- ts(balt_TRU, start = 1, end = 150)

# Plots
plot(balt_TRU)

# Model
fit.TRU <- auto.arima(balt_TRU)
fit.TRU

# Forecast
pred.TRU <- predict(fit.TRU, n.ahead = 5)
pred.TRU <- forecast(fit.TRU, h = 5)
pred.TRU
plot(pred.TRU)


### District 15: WD

# Data
balt %>%
  dplyr::filter(District == "WD") %>%
  select(number) -> balt_WD

balt_WD <- ts(balt_WD, start = 1, end = 150)

# Plots
plot(balt_WD)

# Model
fit.WD <- auto.arima(balt_WD)
fit.WD

# Forecast
pred.WD <- predict(fit.WD, n.ahead = 5)
pred.WD <- forecast(fit.WD, h = 5)
pred.WD
plot(pred.WD)


### More Baltimore Analysis

