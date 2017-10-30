library(RCurl)
library(jsonlite)
library(Rfast)
library(ggmap)
library(tidyverse)
library(stringr)

CleanerSeattle = function(df){
  # Recode Event Number and Offense Number to factor
  df$CAD.Event.Number = as.factor(df$CAD.Event.Number)
  df$General.Offense.Number = as.factor(df$General.Offense.Number)
  
  # Convert date time character variables into actual date-times
  df$Event.Clearance.Date = strptime(df$Event.Clearance.Date, format = "%m/%d/%Y %I:%M:%S %p")
  df$At.Scene.Time        = strptime(df$At.Scene.Time, format = "%m/%d/%Y %I:%M:%S %p")
  
  return(df)
}

Seattle = read.csv("Seattle_Police_Department_911_Incident_Response.csv", as.is = TRUE, strip.white = TRUE)

#############################################################
#                            Months                         #
#############################################################

ds=CleanerSeattle(Seattle)


View(ds)

groupedDS=Seattle
groupedDS$Date=substr(groupedDS$Event.Clearance.Date,1,10)
groupedDS$Date=as.Date(strptime(groupedDS$Date, format="%m/%d/%Y"))
groupedDS=group_by(groupedDS, Date)
groupedDS=dplyr::summarise(groupedDS, Ncalls=n())
groupedDS$Day.of.Week=weekdays(groupedDS$Date)
groupedDS$Month=months(groupedDS$Date)
groupedDS$Day.of.Week=as.factor(groupedDS$Day.of.Week)
groupedDS$Month=as.factor(groupedDS$Month)
#groupedDS$Date=str_replace_all(groupedDS$Date, "-", "") #only needed for rwunderground

View(groupedDS)

plot(groupedDS$Date, groupedDS$Ncalls, ylim = c(0,2000))
lm(Ncalls ~ . -Date, data=groupedDS)
#baselines are Friday and April

#There are missing time periods in our dataset!!

#############################################################
#                          Hours of Day                     #
#############################################################

hourDS=ds
hourDS$hour=substr(hourDS$Event.Clearance.Date,1,13)
hourDS$hour=str_replace_all(hourDS$hour," ", "")
hourDS$hour=str_replace_all(hourDS$hour,"-", "")
hourDS$Day.of.Week=weekdays(hourDS$Event.Clearance.Date)
hourDS$Month=months(hourDS$Event.Clearance.Date)

rm(smallDS)
smallDS=cbind(hourDS$hour,hourDS$Day.of.Week,hourDS$Month)
colnames(smallDS)=c("date_hour","day","month")
smallDS=as.data.frame(smallDS)

smallDS=group_by(smallDS, date_hour)
smallDS=dplyr::summarise(smallDS, Ncalls=n())
smallDS$day_of_week=weekdays(as.Date(substr(smallDS$date_hour,1,8),format ="%Y%m%d"))
smallDS$month=months(as.Date(substr(smallDS$date_hour,1,8),format ="%Y%m%d"))
smallDS$hour=substr(smallDS$date_hour,9,10)

smallDS$day_of_week=as.factor(smallDS$day_of_week)
smallDS$month=as.factor(smallDS$month)
smallDS$hour=as.factor(smallDS$hour)


lm(Ncalls ~ . -date_hour, data=smallDS)


#############################################################
#                         Weather                           #
#############################################################

library(rnoaa)
#ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt

station_data = ghcnd_stations()
Seattle_station=filter(station_data, latitude<48, latitude>47,
                       state=="WA", last_year==2017)
View(Seattle_station)

weather_merp=ghcnd_search("USW00024233", var = c("TAVG", "PRCP"), date_min = "2010-07-18", date_max = "2017-10-22")
#weather_merp=ghcnd_search("USC00458278", var = "all", date_min = "2010-07-18", date_max = "2017-10-22")

rm(weatherDS)
weatherDS=groupedDS
weatherDS=weatherDS[-2294,]
weatherDS$temp_C=NA
weatherDS$prcp_mm=NA

for(i in 1:nrow(weatherDS)){
  for(j in 1:nrow(weather_merp[[1]])){
    if(as.numeric(weatherDS$Date[i])==as.numeric(weather_merp[[1]][j,3])){
      weatherDS$temp_C[i]=weather_merp[[1]][j,2]/10
    }
  }
}

for(i in 1:nrow(weatherDS)){
  for(j in 1:nrow(weather_merp[[2]])){
    if(as.numeric(weatherDS$Date[i])==as.numeric(weather_merp[[2]][j,3])){
      weatherDS$prcp_mm[i]=weather_merp[[2]][j,2]/10
    }
  }
}

lm(Ncalls ~ . -Date, data=weatherDS)