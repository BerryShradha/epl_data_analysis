#######################################################################
##########################  DATA PREPARATION ##########################

# 1. Read epl.csv, weather.csv and dates_location_mapping.xlsx
# 2. Sync data types of joining attributes
# 3. Join datasets
# 4. Cleanup the merged dataset. Check for NAs, duplicates, outliers
# 5. Perform feature selection
# 6. Perform exploratory analysis

#######################################################################

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

#Bring all joining column datatypes in sync
mapping_data$Date <- as.character.Date(mapping_data$Date)
epl_data$Date <- as.character.Date(epl_data$Date)
epl_data$HomeTeam <- as.character.factor(epl_data$HomeTeam)
weather_data$daily.time <- as.character.Date(weather_data$daily.time)

library("dplyr")
#Merge weather with epl using mapping data
epl_mapping_data <- left_join(epl_data, mapping_data, by = c("Date" = "Date", "HomeTeam" = "Home Team"))
str(epl_mapping_data)
merged_data <- left_join(epl_mapping_data, weather_data,
                          by = c("Date" = "daily.time", "Latitude" = "latitude", "Longitude" = "longitude"))
str(merged_data)
#write.table(merged_data, "merged.csv", sep = ",", row.names = F)

#Check for duplicates
table(duplicated(merged_data))

#Check for missing data
merged_data_na <- apply(is.na(merged_data), 2, sum) #Check which attributes have NAs. 
merged_data_na #Shows 19 weather records are consistently NA.  
subset(merged_data, is.na(merged_data$daily.summary)) != 0 #Check all records where daily.summary is NA. Shows all matches from 0910 for Fulham

