#Project Storm WorkSpace

#Two Main Questions:
#1. Across United States, which type of events(EVTYPE) are most harmful with respect to 
#population health?

#2. Across US, which types of events have the greatest economic consequences?

setwd("~/R Material/Coursera/Course 5 Reproducible Research/Week 4/Project Storm/repdata_data_StormData.csv")
WeatherDatabase <- read.csv("repdata_data_StormData.csv", stringsAsFactors = FALSE)

library(dplyr)
#list of variables important to the analysis
UsefulColumns <- c("EVTYPE", "STATE","BGN_DATE", "BGN_TIME", "TIME_ZONE",
                   "END_DATE", "END_TIME","FATALITIES", "INJURIES", "PROPDMG",
                   "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")
#Retain only the useful variables
WeatherDatabase <- select(WeatherDatabase, UsefulColumns)

#First, change BGN_DATE from a factor class to a Date Class
WeatherDatabase$BGN_DATE <- as.Date(WeatherDatabase$BGN_DATE, format = "%m/%d/%Y")
WeatherDatabase <- filter(WeatherDatabase, BGN_DATE >= "1996-01-01")
