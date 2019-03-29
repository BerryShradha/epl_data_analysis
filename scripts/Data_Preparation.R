#######################################################################
##########################  DATA PREPARATION ##########################

# 1. Read epl.csv, weather.csv and dates_location_mapping.xlsx
# 2. Sync data types of joining attributes
# 3. Join datasets
# 4. Cleanup the merged dataset. Check for NAs, duplicates, outliers
# 5. Perform feature selection
# 6. Perform exploratory analysis

#######################################################################

# 1. Read epl.csv, weather.csv and dates_location_mapping.xlsx

setwd("C:/Users/Shradha/Desktop/Study/2. Big Data Analytics/Coursework/epl_data_analysis/data")

#Function to combine part csvs to whole
combine <- function(path, csv_name) {
  setwd(path)
  filenames <- list.files()
  data <- Reduce(rbind, lapply(filenames, read.csv))
  #write.csv(data, csv_name, row.names = F) 
  return (data)
}

#Load epl data. Combine multiple files into one (one-time execution)
epl_data <- combine("./EPL_Results", "epl.csv")
str(epl_data)

setwd("C:/Users/Shradha/Desktop/Study/2. Big Data Analytics/Coursework/epl_data_analysis/data")

#Load weather data. Merge multiple files into one (one-time execution)
weather_data <- combine("./Weather", "weather.csv")
str(weather_data)

setwd("C:/Users/Shradha/Desktop/Study/2. Big Data Analytics/Coursework/epl_data_analysis/data")

#install.packages("readxl")
library("readxl")
#Load mapping csv to be used to merge epl and weather data
mapping_data <- read_excel("Dates_location_mapping.xlsx")
str(mapping_data)

# 2. Sync data types of joining attributes

#Bring all joining column datatypes in sync
mapping_data$Date <- as.character.Date(mapping_data$Date)
epl_data$Date <- as.character.Date(epl_data$Date)
epl_data$HomeTeam <- as.character.factor(epl_data$HomeTeam)
weather_data$daily.time <- as.character.Date(weather_data$daily.time)

# 3. Join datasets

library("dplyr")
#Merge weather with epl using mapping data
epl_mapping_data <- left_join(epl_data, mapping_data, by = c("Date" = "Date", "HomeTeam" = "Home Team"))
str(epl_mapping_data)
merged_data <- left_join(epl_mapping_data, weather_data,
                          by = c("Date" = "daily.time", "Latitude" = "latitude", "Longitude" = "longitude"))
str(merged_data)
#write.table(merged_data, "merged.csv", sep = ",", row.names = F)

# 4. Cleanup the merged dataset. Check for NAs, duplicates, outliers

#Check for duplicates
table(duplicated(merged_data))

#Check for missing data
merged_data_na <- apply(is.na(merged_data), 2, sum) #Check which attributes have NAs. 
merged_data_na #Shows 19 weather records are consistently NA.  
#subset(merged_data, is.na(merged_data$daily.summary)) != 0 #Check all records where daily.summary is NA. Shows all matches from 0910 for Fulham
#Fixed above problem. Problem with coordinates of the club. Fixed.
#Handle at individual columns that have missing data
colnames(merged_data)[apply(merged_data, 2, anyNA)]

## a. daily.pressure
sum(is.na(merged_data$daily.pressure)) #2 NAs

## b. daily.cloudcover
sum(is.na(merged_data$daily.cloudCover)) #41 NAs

## c. daily.visibility
sum(is.na(merged_data$daily.visibility)) #23 NAs

#Replace NAs for pressure,cloudcover,visibility with corresponding value at nearest coordinate on that day, if daily.summary is same
#install.packages("stringdist")
library(stringdist)
replace_na <- function(merged_data) {
  for (i in 1:dim(merged_data)[1]) {
    if (is.na(merged_data$daily.cloudCover[i]) | 
        is.na(merged_data$daily.visibility[i]) | 
        is.na(merged_data$daily.pressure[i])) {
      records <- merged_data[merged_data$Date == merged_data$Date[i] & 
                               merged_data$daily.icon == merged_data$daily.icon[i] & 
                               merged_data$home.team != merged_data$home.team[i],]
      if (nrow(records) > 0)
        record <- records[which.min(Reduce('+',Map(stringdist,records$Coordinates, merged_data$Coordinates[i], method='jaccard'))),]
      if (nrow(record) > 0) {
        if (is.na(merged_data$daily.cloudCover[i]) & !is.na(record$daily.cloudCover))
          merged_data$daily.cloudCover[i] = record$daily.cloudCover
        if (is.na(merged_data$daily.visibility[i]) & !is.na(record$daily.visibility))
          merged_data$daily.visibility[i] <- record$daily.visibility
        if (is.na(merged_data$daily.pressure[i]) & !is.na(record$daily.pressure))
          merged_data$daily.pressure[i] <- record$daily.pressure
      }
    }
  }
  return(merged_data)
}

merged_data_clean <- replace_na(merged_data)
merged_data_na <- apply(is.na(merged_data_clean), 2, sum) #Check which attributes have NAs. 
merged_data_na 

#Still 2, 14, 14 records are NA. This is because there are no other match records on this day that have matching criteria
#To handle these NAs, take an average of the attribute for the location, with same daily.icon
library(dplyr)
replace_na_v2 <- function(merged_data_clean) {
  for (i in 1:dim(merged_data_clean)[1]) {
    if (is.na(merged_data_clean$daily.cloudCover[i]) | 
        is.na(merged_data_clean$daily.visibility[i]) | 
        is.na(merged_data_clean$daily.pressure[i])) {
      avgs <- merged_data_clean %>% 
        filter(daily.icon == merged_data_clean$daily.icon[i], 
               home.team == merged_data_clean$home.team[i], 
               Date != merged_data_clean$Date[i]) %>%
        select(daily.cloudCover, daily.visibility, daily.pressure) %>% 
        summarize(avg_cloudcover = mean(daily.cloudCover, na.rm = TRUE), 
                  avg_visiblity = mean(daily.visibility, na.rm = TRUE), 
                  avg_pressure = mean(daily.pressure, na.rm = TRUE))
      if (is.na(merged_data_clean$daily.cloudCover[i]))
        merged_data_clean$daily.cloudCover[i] <- avgs$avg_cloudcover
      if (is.na(merged_data_clean$daily.visibility[i]))
        merged_data_clean$daily.visibility[i] <- avgs$avg_visiblity
      if (is.na(merged_data_clean$daily.pressure[i]))
        merged_data_clean$daily.pressure[i] <- avgs$avg_pressure
      }
    }
  return(merged_data_clean)
}

merged_data_clean <- replace_na_v2(merged_data_clean)
merged_data_na <- apply(is.na(merged_data_clean), 2, sum) #Check which attributes have NAs. cloudcover 8, visibility 2
merged_data_na 

#Remove the two row that have NA in cloudcover and visibility
colnames(merged_data_clean)[apply(merged_data_clean, 2, anyNA)]
merged_data_clean <- merged_data_clean[complete.cases(merged_data_clean), ]
merged_data_na <- apply(is.na(merged_data_clean), 2, sum) #Check which attributes have NAs. cloudcover 8, visibility 2
merged_data_na 

## d. daily.precipAccumulation
sum(is.na(merged_data$daily.precipAccumulation)) #3375 NAs
#Almost all rows have NA. Remove this attribute 
merged_data_clean <- within(merged_data_clean, rm(daily.precipAccumulation))
str(merged_data_clean)


# 5. Perform feature selection (removing attributes) and feature extraction (deriving new attributes)
str(merged_data_clean)
#Removing "Referee, FTHG, FTAG, id, club, location, coordinates, Latitude, Longitude, daily.sunrisesTime, daily.sunsetTime,
#daily.precipIntensityMaxTime, daily.temperatureHighTime, daily.temperatureLowTime,id, daily.apparentTemperatureHighTime,
#daily.apparentTemperatureLowTime, daily.windGustTime, daily.uvIndexTime, daily.temperatureMinTime, daily.temperatureMaxTime,
#daily.apparentTemperatureMinTime, daily.apparentTemperatureMaxTime, location, home.team
#drop columns using dplyr function drop()
final_data <- select (merged_data_clean,-c(Referee, FTHG, FTAG, Id, Club, Location, Coordinates, Latitude, Longitude, 
                                           daily.sunriseTime, daily.sunsetTime, daily.precipIntensityMaxTime, 
                                           daily.temperatureHighTime, daily.temperatureLowTime,id, 
                                           daily.apparentTemperatureHighTime,daily.apparentTemperatureLowTime, 
                                           daily.windGustTime, daily.uvIndexTime, daily.temperatureMinTime, 
                                           daily.temperatureMaxTime,daily.apparentTemperatureMinTime, 
                                           daily.apparentTemperatureMaxTime, location, home.team))
str(final_data)


#Outlier detection


# 6. Perform exploratory analysis
str(merged_data_clean)
boxplot(merged_data_clean$HTHG)$out
hist(merged_data_clean$HTHG)
boxplot(merged_data_clean$HTAG)$out
hist(merged_data_clean$HTAG)