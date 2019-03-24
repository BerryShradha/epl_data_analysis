#######################################################################
##########################  DATA PREPARATION ##########################

# 1. Read epl.csv, weather.csv and dates_location_mapping.xlsx
# 2. Sync data types of joining attributes
# 3. Join datasets
# 4. Cleanup the merged dataset. Check for NAs, duplicates, outliers
# 5. Perform feature selection
# 6. Perform exploratory analysis

#######################################################################


#Load epl data. Combine multiple files into one (one-time execution)
setwd("./EPL_Results")
epl_filenames <- list.files()
epl_data <- Reduce(rbind, lapply(epl_filenames, read.csv))
write.csv(epl_data, "epl.csv", row.names = F) 

#Load weather data. Merge multiple files into one (one-time execution)
setwd("./Weather")
weather_filenames <- list.files()
weather_data <- Reduce(rbind, lapply(weather_filenames, read.csv))
write.csv(weather_data, "weather.csv", row.names = F)

#load consolidated epl data and consolidated weather data ans inspect
setwd("./Data")
epl_data <- read.csv("epl.csv", header = T, stringsAsFactors = F)
weather_data <- read.csv("weather.csv", header = T, stringsAsFactors = F)
str(epl_data)
str(weather_data)

#install.packages("readxl")
library("readxl")
#Load mapping csv to be used to merge epl and weather data
mapping_data <- read_excel("Dates_location_mapping.xlsx")
str(mapping_data)

#Bring all joining column datatypes in sync
#colnames(mapping_data)[3] <- "HomeTeam"
mapping_data$Date <- as.character.Date(mapping_data$Date)
epl_data$Date <- as.character.Date(epl_data$Date)
#colnames(weather_data)[1] <- "Date"
weather_data$Date <- as.character.Date(weather_data$Date)
#colnames(weather_data)[39] <- "Id"
#colnames(weather_data)[40] <- "Location"

library("dplyr")
#Merge weather with epl results
epl_mapping_data <- left_join(epl_data, mapping_data, by = c("Date" = "Date", "HomeTeam" = "Home Team"))
str(epl_mapping_data)
str(weather_data)
merged_data <- left_join(epl_mapping_data, weather_data,
                          by = c("Date" = "daily.time", "Latitude" = "latitude", "Longitude" = "longitude"))
str(merged_data)
write.table(merged_data, "merged.csv", sep = ",", row.names = F)

#Exploratory Analysis of EPL data
cor(epl_data)
