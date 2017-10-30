##### Baltimore Analysis #####

#############################################

# Loading libraries
library(RCurl)
library(jsonlite)
library(Rfast)
library(ggmap)
library(lubridate)
library(tidyverse)
library(stringr)
library(RSNNS)
library(nnet)
library(neuralnet)

#############################################

##### Initial Read In of Data #####

# Reading in data
balt = read.csv("balt_data.csv", header = T, 
                as.is = TRUE, strip.white = TRUE)

# Looking at data
head(balt)
dim(balt)

# Renaming variables
colnames(balt) <- c("DateTime", "Priority", "District",
                    "Description", "CallNumber", "Address",
                    "Location")

# Looking at data again
str(balt)

#############################################

##### Creating lattitude/longtitude variable #####

# Getting rid of brackets
balt$Location <- gsub(pattern = '[()]', 
                            replacement = '', x = balt$Location)

# Separating lattitude and longtitude
split_coords <- as.data.frame(str_split(string = balt$Location, 
                          pattern = ',', n = 2, simplify = TRUE))

# Renaming co-ordinate data set
colnames(split_coords) <- c("Lattitude", "Longitude")

# Combining back with original data
balt2 <- data.frame(cbind(balt, split_coords))

# Getting rid of useless variable
balt2 <- subset(balt2, select = -c(Location))

#############################################

##### Editing Time Variables #####

# Creating time variables
balt2$DateTime <- mdy_hms(balt2$DateTime)
balt2$Time <- substr(balt2$DateTime, 12, 19)

# Looking at time variable
head(balt2$Time)
head(balt2$DateTime)

# Looking at arithmetic
balt2$DateTime[1000000] - balt2$DateTime[999991]

# Creating years, monthly, weekly variables
Week <- data.frame(Dates = balt2$DateTime, 
                  Week = format(balt2$DateTime, format = "%W"))

Month <- data.frame(Dates = balt2$DateTime, 
                    Month = format(balt2$DateTime, format = "%m"))

Year <- data.frame(Dates = balt2$DateTime,
                   Year = format(balt2$DateTime, format = "%Y"))
head(Year)

# Combining back with original data set
balt2 <- data.frame(cbind(balt2, 
            Month = Month$Month, Week = Week$Week,
            Year = Year$Year))

#############################################

##### Looking at Priority Variable #####

# Looking at high priority crimes
balt_high <- balt2 %>% 
  filter(Priority == "High") %>% 
  select(Priority, Description)

# Getting all actual high priority crimes
High_Priority <- unique(balt_low$Description)[c(4, 6, 7, 10, 11, 
                               15, 16, 17, 20, 28,
                               70, 72, 74, 85,
                               95, 96, 98, 99, 100,
                               101, 105, 115, 116,
                               117, 119, 120, 121,
                               124, 131, 132, 133,
                               139, 140, 144, 145,
                               148, 158, 176, 174,
                               197, 204, 208, 210,
                               211, 213, 214, 220,
                               223, 231, 234, 235,
                               244, 245, 256, 260,
                               264, 277, 281, 294,
                               303, 304, 307, 318,
                               310, 311, 312, 344,
                               327, 329, 334, 352,
                               359, 368, 372, 380,
                               384, 404, 401, 402,
                               411, 422, 424, 425,
                               429, 432, 438, 449,
                               457, 458, 459, 461,
                               470, 472, 478, 477,
                               493, 512, 533, 565,
                               578, 587, 612, 615,
                               623, 641, 660, 672,
                               684, 690, 701, 711,
                               724, 730, 739, 741,
                               765, 800, 803)]

# Creating very high priority variable
balt3 <- balt2 %>% 
  mutate(Priority = ifelse(balt2$Description %in% High_Priority,
                           "Very High", Priority))

# Creating factor for priority
balt3$Priority <- as.factor(balt3$Priority)

# Looking at priority
levels(balt3$Priority)

# Subsetting out emergency priority
balt_emergency <- balt2 %>% 
  filter(Priority == "Emergency") %>% 
  select(Priority, Description)

# Looking at emergency priorities
balt_emergency

### This looks like all the times that back up 
### was needed so this might be worth looking at
### When we make our predictions

#############################################

##### Transforming data set #####

# Getting a count by week and also by week/year
balt_grouped <- balt3 %>% 
  group_by(Week, Year, Priority) %>%
  summarise(number = n())

balt_grouped2 <- balt3 %>% 
  group_by(Week, Priority) %>%
  summarise(number = n())

# Getting as data frame for both
balt_grouped <- as.data.frame(balt_grouped)
balt_grouped2 <- as.data.frame(balt_grouped2)

# Getting rid of all observations with "" for priority for both
balt_grouped <- balt_grouped %>% 
  filter(Priority != "")

balt_grouped2 <- balt_grouped2 %>% 
  filter(Priority != "")

# Let's look at only very high for both
balt_grouped_vh <- balt_grouped %>% 
  filter(Priority == "Very High")

balt_grouped2_vh <- balt_grouped2 %>% 
  filter(Priority == "Very High")

# Looking at plots for both
ggplot(data = balt_grouped_vh, aes(x = Week, y = number)) +
  geom_point(aes(color = balt_grouped_vh$Year))

ggplot(data = balt_grouped2_vh, aes(x = time(Week), y = number)) +
  geom_point()

### It makes sense to get rid of the first and 
### last week because that is holiday season and this
### is usually associated with a specific kind of 
### behaviour amongst communities

# Transforming data
Balt_Spread <- balt_grouped2 %>% spread(Week, number)

# New variable names
weeks <- NULL
for(i in 1:53){
  weeks[i] <- paste("Week", i, sep ="")
}

# Adding them to data set
colnames(Balt_Spread)[2:54] <- weeks

#############################################

##### Creating Model #####

model2 <- elman(Balt_Spread$Week25, Balt_Spread[,c(26)],
               size = c(10, 10),
               learnFuncParams = c(0.001),
               maxit = 500,
               linOut = T)

model2$fitted.values
Balt_Spread$Week25

# Balt_Spread_n <- data.frame(cbind(Priority = Balt_Spread[,1], scale(Balt_Spread[,-1])))
# 
# gene_ANN2 <- nnet(Balt_Spread[, -c(26)], ideal4, size = 10, softmax = T, MaxNWts = 4714)
# 
# test_pred2 <- predict(gene_ANN2, testData[, -c(1:4)], type = "class")
# 

