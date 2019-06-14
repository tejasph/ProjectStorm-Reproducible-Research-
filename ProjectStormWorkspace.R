#Project Storm WorkSpace

#Two Main Questions:
#1. Across United States, which type of events(EVTYPE) are most harmful with respect to 
#population health?

#2. Across US, which types of events have the greatest economic consequences?

setwd("~/R Material/Coursera/Course 5 Reproducible Research/Week 4/Project Storm/repdata_data_StormData.csv")
WeatherDatabase <- read.csv("repdata_data_StormData.csv", stringsAsFactors = FALSE)

library(dplyr)

#EVTYPE Cleaning
EVClean <- function(x){
      print(paste("Previous Unique Events:", length(unique(x))))
      x <- toupper(x)
      x <- trimws(x, which = c("both", "left", "right"))
      x <- gsub("BLIZZARD SUMMARY", "BLIZZARD", x)
      x <- gsub("COASTAL.*", "COASTAL FLOOD",x)
      x <- gsub(".*DUST DEV.*", "DUST DEVIL", x)
      x <- gsub(".*SMOKE*.","DENSE SMOKE", x)
      x <- gsub(".*DUST$", "DUST STORM", x)
      x <- gsub("^TSTM.*", "THUNDERSTORM WIND", x) # need to account for thunderstorm wind
      x <- gsub("HURRICANE.*", "HURRICANE", x)
      x[grepl("TYPHOON",x)] <- "HURRICANE"
      x <- gsub("^VOLCANIC.*", "VOLCANIC ASH", x)
      x[grepl("MARINE\\s[T]", x)] <- "MARINE THUNDERSTORM WIND"
      x[grepl("(?<!THUNDERSTORM\\s)(WIND)", x, perl = TRUE)] <- "WIND"
      x[grepl("SURF", x)] <- "HIGH SURF"
      x[grepl("FUNNEL",x)] <- "FUNNEL CLOUD"
      x[grepl("(?<=STORM\\s)(WIND)", x, perl = TRUE)] <- "THUNDERSTORM WIND"
      x[grepl("FLOOD", x)] <- "FLOODING"
      x[grepl("DEBRIS",x)] <- "DEBRIS FLOW"
      x[grepl("(?<!FREEZING\\s)(FOG)",x, perl= TRUE)] <- "DENSE FOG"
      x[grepl("HEAT", x)]<- "HEAT"
      x[grepl("HOT", x)]<- "HEAT"
      x[grepl("WARM", x)]<- "HEAT"
      x[grepl("HAIL",x)] <- "HAIL"
      x[grepl("ICE",x)] <- "ICE STORM"
      x[grepl("RIP",x)] <- "RIP CURRENT"
      x[grepl("SLEE",x)] <- "SLEET"
      x[grepl("WATERSPOUT",x)] <- "WATERSPOUT"
      x[grepl("FIRE",x)] <- "WILDFIRE"
      x[grepl("LAKE",x)] <- "LAKE-EFFECT SNOW"
      x[grepl("(?<!LAKE-EFFECT\\s)(SNOW)", x, perl = TRUE)] <- "HEAVY SNOW" 
      x[grepl("(?!.*STORM)(WINT)", x, perl= TRUE)] <- "WINTER WEATHER"
      x[grepl("COLD",x)] <- "EXCESSIVE COLD"
      x[grepl("COOL",x)] <- "EXCESSIVE COLD"
      x[grepl("DRY",x)] <- "DROUGHT"
      x[grepl("DRIEST",x)] <- "DROUGHT"
      
      print(paste("Unique Events:", length(unique(x))))
      return(x)
      
}

#list of variables important to the analysis
UsefulColumns <- c("EVTYPE", "STATE","BGN_DATE", "BGN_TIME", "TIME_ZONE",
                   "END_DATE", "END_TIME","FATALITIES", "INJURIES", "PROPDMG",
                   "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")
EventNames <- toupper(c("Astronomical Low Tide", "Avalanche", "Blizzard","Coastal Flood","Debris Flow",
                "Dense Smoke", "Drought", "Dust Devil", "Dust Storm", "Excessive Heat", 
                "Flash Flood", "Flood", "Frost/Freeze", "Funnel Cloud", "Hail", "Heat", "Heavy Rain",
                "Heavy Snow", "High Surf", "Wind", "Hurricane", "Ice Storm", "Lake-Effect Snow", 
                "Flooding", "Lightning", "Marine Thunderstorm Wind", "Rip Current", "Seiche",
                "Sleet", "Storm Surge/Tide", "Thunderstorm Wind", "Tornado","Tropical Depression",
                "Tropical Storm", "Tsunami", "Volcanic Ash", "Waterspout", "Wildfire", "Winter Storm",
                "Winter Weather"))
#Retain only the useful variables
WeatherDatabase <- select(WeatherDatabase, UsefulColumns)

#First, change BGN_DATE from a factor class to a Date Class
WeatherDatabase$BGN_DATE <- as.Date(WeatherDatabase$BGN_DATE, format = "%m/%d/%Y")
WeatherDatabase <- filter(WeatherDatabase, BGN_DATE >= "1996-01-01")
WeatherDatabase$EVTYPE <- toupper(WeatherDatabase$EVTYPE)
WeatherDatabase <- WeatherDatabase[-grep("^SUMMARY", WeatherDatabase$EVTYPE),]

WeatherDatabase$EVTYPE <- EVClean(WeatherDatabase$EVTYPE)
RemovalIndex = integer()
print(paste("Before:", as.character(length(WeatherDatabase$EVTYPE))))

for (i in c(1:length(WeatherDatabase$EVTYPE))){
      for (x in c(1:length(EventNames))){
            if (WeatherDatabase$EVTYPE[i] == EventNames[x]){
                  break
            } else if (x == length(EventNames)) {
                  RemovalIndex = c(RemovalIndex, i)
                  break
                 
            }
      }
}

WeatherDatabase <- WeatherDatabase[-RemovalIndex,]
print(unique(WeatherDatabase$EVTYPE))

