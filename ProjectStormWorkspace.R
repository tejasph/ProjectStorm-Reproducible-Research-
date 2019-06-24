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
      
      
      return(x)
      
}

ExpConvert <- function(x) {
      if (grepl("K", x)== TRUE){
            x <- 1000
      } else if (grepl("M",x)== TRUE){
            x <- 10^6
      } else if (grepl("B", x)== TRUE){
            x <- 10^9
      } else if (grepl("H", x)== TRUE){
            x <- 100
      } else if (grepl("[012345678]", x)== TRUE){
            x <- (10^(as.numeric(x)))
      } else {
            x <- NA
      }
      return(x)
}

DmgCombine <- function(Dmg, Exp){
      
      for (i in c(1:length(Dmg))) {
          Dmg[i] <- Dmg[i]*Exp[i] 
             
      }
      return (Dmg)
}

#finds the top 5 for one of the parameter categories: fatalities, injuries, cropdmg, propdmg
TopFive <- function(Frame, Category){
      Frame <- Frame[order(-Frame[,2]),]
      print(head(Frame))
      return(Frame[1:5,1])
}
#Removes NA Values from a given dataframe
CheckNA <- function(NAFrame){
      RemovedNA <- NAFrame[!is.na(NAFrame[,3]),]
      return(RemovedNA)
}



#list of variables important to the analysis
UsefulColumns <- c("EVTYPE","BGN_DATE", "FATALITIES", "INJURIES", "PROPDMG",
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
WeatherDatabase$PROPDMGEXP <- toupper(WeatherDatabase$PROPDMGEXP)
WeatherDatabase$PROPDMGEXP <- sapply(WeatherDatabase$PROPDMGEXP, ExpConvert)
WeatherDatabase <- mutate(WeatherDatabase, PROPDMG = DmgCombine(PROPDMG,PROPDMGEXP))

WeatherDatabase$CROPDMGEXP <- toupper(WeatherDatabase$CROPDMGEXP)
WeatherDatabase$CROPDMGEXP <- sapply(WeatherDatabase$CROPDMGEXP, ExpConvert)
WeatherDatabase <- mutate(WeatherDatabase, CROPDMG = DmgCombine(CROPDMG,CROPDMGEXP))

#HealthFrame <- with(WeatherDatabase,data.frame(EventType = EVTYPE, BGN_DATE = BGN_DATE,
                                               #Fatalities =FATALITIES, Injuries = INJURIES,
                                               #PropertyDmg = PROPDMG, CropDmg = CROPDMG, stringsAsFactors = FALSE))
WeatherDatabase$BGN_DATE <- as.numeric(substring(as.character(WeatherDatabase$BGN_DATE),1,4))

FatalityFrame <- CheckNA(select(WeatherDatabase, EVTYPE:FATALITIES))
InjuryFrame <- CheckNA(select(WeatherDatabase, c(EVTYPE, BGN_DATE, INJURIES)))
PropertyFrame <- CheckNA(select(WeatherDatabase, c(EVTYPE, BGN_DATE, PROPDMG)))
CropFrame <- CheckNA(select(WeatherDatabase, c(EVTYPE, BGN_DATE, CROPDMG)))

FatalitySumFrame <- group_by(FatalityFrame, EVTYPE) %>% 
     summarise(sum(FATALITIES))

InjurySumFrame <- group_by(InjuryFrame, EVTYPE) %>% 
      summarise(sum(INJURIES))
PropertySumFrame <- group_by(PropertyFrame, EVTYPE) %>% 
      summarise(sum(PROPDMG))
CropSumFrame <- group_by(CropFrame, EVTYPE) %>% 
      summarise(sum(CROPDMG))

names(FatalitySumFrame) <- c("EVTYPE","FATALITIES")
names(InjurySumFrame) <- c("EVTYPE","INJURIES")
names(PropertySumFrame) <- c("EVTYPE","PROPDMG")
names(CropSumFrame) <- c("EVTYPE","CROPDMG")



#find top five values for each category
TopFatalities <- TopFive(FatalitySumFrame, "FATALITIES")$EVTYPE
TopInjuries <- TopFive(InjurySumFrame, "INJURIES")$EVTYPE
TopPropertyDmg <- TopFive(PropertySumFrame, "PROPDMG")$EVTYPE
TopCropDmg <- TopFive(CropSumFrame, "CROPDMG")$EVTYPE

FatalityFrame <- group_by(FatalityFrame, EVTYPE, BGN_DATE)%>% summarise(sum(FATALITIES))
names(FatalityFrame) <- c("EVTYPE","BGN_DATE","Fatalities")
FatalityFrame <- FatalityFrame[FatalityFrame$EVTYPE %in% TopFatalities, c(1:3)]

InjuryFrame <- group_by(InjuryFrame, EVTYPE, BGN_DATE) %>% summarise(sum(INJURIES))
names(InjuryFrame) <- c("EVTYPE", "BGN_DATE", "Injuries")
InjuryFrame <- InjuryFrame[InjuryFrame$EVTYPE %in% TopInjuries, c(1:3)]

PropertyFrame <- group_by(PropertyFrame, EVTYPE, BGN_DATE) %>% summarise(sum(PROPDMG))
names(PropertyFrame) <- c("EVTYPE", "BGN_DATE", "PropDmg")
PropertyFrame <- PropertyFrame[PropertyFrame$EVTYPE %in% TopPropertyDmg, c(1:3)]

CropFrame <- group_by(CropFrame, EVTYPE, BGN_DATE) %>% summarise(sum(CROPDMG))
names(CropFrame) <- c("EVTYPE", "BGN_DATE", "CropDmg")
CropFrame <- CropFrame[CropFrame$EVTYPE %in% TopCropDmg, c(1:3)]


#YearlyTable <- group_by(WeatherDatabase, EVTYPE, BGN_DATE) %>% summarise(sum(FATALITIES), sum(INJURIES), sum(PROPDMG), sum(CROPDMG))
#names(YearlyTable) <- c("EventType", "BgnDate", "Fatalities", "Injuries", "PropertyDmg", "CropDmg")

#Fatality Analysis
#FatalityFrame <- YearlyTable[YearlyTable$EventType %in% TopFatalities, c(1:3)]

#g <- ggplot(FatalityFrame, aes(x = BgnDate,y = Fatalities, col = EventType)) 
#g <- g + geom_line(size = 2)

#
