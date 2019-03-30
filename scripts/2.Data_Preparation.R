#######################################################################
##########################  DATA PREPARATION ##########################

# 1. Read epl.csv, weather.csv and dates_location_mapping.xlsx
# 2. Sync data types of joining attributes
# 3. Join datasets
# 4. Cleanup the merged dataset. Check for NAs, duplicates
# 5. Perform feature selection
# 6. Analyse outliers
# 7. Perform exploratory analysis

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

## a. daily.precipAccumulation
sum(is.na(merged_data$daily.precipAccumulation)) #3375 NAs
#Almost all rows have NA. Remove this attribute 
merged_data_clean <- within(merged_data, rm(daily.precipAccumulation))
str(merged_data_clean)

## b. daily.pressure
sum(is.na(merged_data_clean$daily.pressure)) #2 NAs

## c. daily.cloudcover
sum(is.na(merged_data_clean$daily.cloudCover)) #41 NAs

## d. daily.visibility
sum(is.na(merged_data_clean$daily.visibility)) #23 NAs

#Replace NAs for pressure,cloudcover,visibility with corresponding value at nearest coordinate on that day, if daily.summary is same
#install.packages("stringdist")
library(stringdist)
replace_na <- function(data) {
  for (i in 1:dim(data)[1]) {
    if (is.na(data$daily.cloudCover[i]) | 
        is.na(data$daily.visibility[i]) | 
        is.na(data$daily.pressure[i])) {
      records <- data[data$Date == data$Date[i] & 
                        data$daily.icon == data$daily.icon[i] & 
                        data$home.team != data$home.team[i],]
      if (nrow(records) > 0)
        record <- records[which.min(Reduce('+',Map(stringdist,records$Coordinates, data$Coordinates[i], method='jaccard'))),]
      if (nrow(record) > 0) {
        if (is.na(data$daily.cloudCover[i]) & !is.na(record$daily.cloudCover))
          data$daily.cloudCover[i] = record$daily.cloudCover
        if (is.na(data$daily.visibility[i]) & !is.na(record$daily.visibility))
          data$daily.visibility[i] <- record$daily.visibility
        if (is.na(data$daily.pressure[i]) & !is.na(record$daily.pressure))
          data$daily.pressure[i] <- record$daily.pressure
      }
    }
  }
  return(data)
}

merged_data_clean <- replace_na(merged_data_clean)
merged_data_na <- apply(is.na(merged_data_clean), 2, sum) #Check which attributes have NAs. 
merged_data_na 

#Still 2, 14, 14 records are NA. This is because there are no other match records on this day that have matching criteria
#To handle these NAs, take an average of the attribute for the location, with same daily.icon
library(dplyr)
replace_na_v2 <- function(data) {
  for (i in 1:dim(data)[1]) {
    if (is.na(data$daily.cloudCover[i]) | 
        is.na(data$daily.visibility[i]) | 
        is.na(data$daily.pressure[i])) {
      avgs <- data %>% 
        filter(daily.icon == data$daily.icon[i], 
               home.team == data$home.team[i], 
               Date != data$Date[i]) %>%
        select(daily.cloudCover, daily.visibility, daily.pressure) %>% 
        summarize(avg_cloudcover = mean(daily.cloudCover, na.rm = TRUE), 
                  avg_visiblity = mean(daily.visibility, na.rm = TRUE), 
                  avg_pressure = mean(daily.pressure, na.rm = TRUE))
      if (is.na(data$daily.cloudCover[i]))
        data$daily.cloudCover[i] <- avgs$avg_cloudcover
      if (is.na(data$daily.visibility[i]))
        data$daily.visibility[i] <- avgs$avg_visiblity
      if (is.na(data$daily.pressure[i]))
        data$daily.pressure[i] <- avgs$avg_pressure
      }
    }
  return(data)
}

merged_data_clean <- replace_na_v2(merged_data_clean)
merged_data_na <- apply(is.na(merged_data_clean), 2, sum) #Check which attributes have NAs. cloudcover 8, visibility 2
merged_data_na 

#Remove the two row that have NA in cloudcover and visibility
colnames(merged_data_clean)[apply(merged_data_clean, 2, anyNA)]
merged_data_clean <- merged_data_clean[complete.cases(merged_data_clean), ]
merged_data_na <- apply(is.na(merged_data_clean), 2, sum) #Check which attributes have NAs. 
merged_data_na 

# 5. Perform feature selection (removing attributes) and feature extraction (deriving new attributes)
str(merged_data_clean)
#Removing "Referee, FTHG, FTAG, id, club, location, coordinates, Latitude, Longitude, daily.sunrisesTime, daily.sunsetTime,
#daily.precipIntensityMaxTime, daily.temperatureHighTime, daily.temperatureLowTime,id, daily.apparentTemperatureHighTime,
#daily.apparentTemperatureLowTime, daily.windGustTime, daily.uvIndexTime, daily.temperatureMinTime, daily.temperatureMaxTime,
#daily.apparentTemperatureMinTime, daily.apparentTemperatureMaxTime, location, home.team
#drop columns using dplyr function drop()
final_data <- select (merged_data_clean,-c(Referee, FTHG, FTAG, Id, Club, Coordinates, Latitude, Longitude, 
                                      daily.sunriseTime, daily.sunsetTime, daily.precipIntensityMaxTime, 
                                      daily.temperatureHighTime, daily.temperatureLowTime,id, 
                                      daily.apparentTemperatureHighTime,daily.apparentTemperatureLowTime, 
                                      daily.windGustTime, daily.uvIndexTime, daily.temperatureMinTime, 
                                      daily.temperatureMaxTime,daily.apparentTemperatureMinTime, 
                                      daily.apparentTemperatureMaxTime, location, home.team))
str(final_data)
# 6. Analyse outliers
#install.packages("qpcR")
# function to check outliers for all attributes
outliers <- function(data){
  numeric_data <- select_if(data, is.numeric)
  result <- NULL
  for(i in 1:length(numeric_data)){
    out <- boxplot.stats(eval(parse(text = paste("data$",names(numeric_data)[i]))))$out
    result <- qpcR:::cbind.na(result, as.data.frame(out))
    names(result)[i] <- names(numeric_data)[i]
  }
  return (result)
}

all_outliers <- outliers(final_data)
all_outliers

#How many outliers
apply(!is.na(all_outliers), 2, sum)

#outlier values
table(all_outliers$HTAG)
boxplot(final_data$HTAG)

# 7. Perform exploratory analysis
library(ggplot2)
library(gridExtra)

# Same day weather difference between locations
par(mar = c(6.5, 6.5, 0.5, 0.5), mgp = c(5, 1, 0))

#Specific clubs, 2018
ggplot(filter(final_data, HomeTeam %in% c("Chelsea", "Liverpool", "Man United") & format(as.Date(Date,'%Y-%m-%d'), '%Y') == 2018), 
       aes(x = Date, y = daily.temperatureMax, group = HomeTeam)) + 
  geom_line(aes(color = HomeTeam), size = 1) +
  theme_minimal()  + xlab("Date") + ylab("Maximum Temperature for the Day")
#All locations, 2018
ggplot(filter(final_data, format(as.Date(Date,'%Y-%m-%d'), '%Y') == 2018), aes(x = Date, y = daily.temperatureMax, group = HomeTeam)) + 
  geom_line(aes(color = Location), size = 1) +
  theme_minimal()  + xlab("Date") + ylab("Maximum Temperature for the Day")

# Same team home and away form
opar <- par()
par(mfrow = c(2,2))
plot(filter(final_data, HomeTeam =="Chelsea")$HTR)
plot(filter(final_data, AwayTeam == "Chelsea")$HTR)
plot(filter(final_data, HomeTeam =="Chelsea")$FTR)
plot(filter(final_data, AwayTeam == "Chelsea")$FTR)
par(opar)

#Label these properly. Left graphs are hometeam, right graphs are awayteam
bar1 <- ggplot(filter(final_data, HomeTeam =="Chelsea"), aes(x = HTR)) + geom_bar()
bar2 <- ggplot(filter(final_data, AwayTeam =="Chelsea"), aes(x = HTR)) + geom_bar()
bar3 <- ggplot(filter(final_data, HomeTeam =="Chelsea"), aes(x = FTR)) + geom_bar()
bar4 <- ggplot(filter(final_data, AwayTeam =="Chelsea"), aes(x = FTR)) + geom_bar()

grid.arrange(bar1, bar2, bar3, bar4, ncol=2)

#Same team form over time -------------Need to fix this. Need to consider away matches.
ggplot(filter(final_data, HomeTeam %in% c("Chelsea", "Liverpool", "Man United") & format(as.Date(Date,'%Y-%m-%d'), '%Y') == 2018), 
       aes(x = Date, y = as.factor(FTR), group = HomeTeam)) + 
  geom_line(aes(color = HomeTeam), size = 1) +
  theme_minimal()  + xlab("Date") + ylab("Maximum Temperature for the Day")

# Distribution of goals
hist1 <- ggplot(final_data, aes(x=HTHG)) + geom_histogram() + 
  geom_histogram(color="black", fill="white")
hist2 <- ggplot(final_data, aes(x=HTAG)) + geom_histogram() + 
  geom_histogram(color="black", fill="white")
grid.arrange(hist1, hist2, ncol=2)

plot(final_data$HTR)
plot(final_data$FTR)

scatter <- ggplot(final_data, aes(HTHG, HTAG, colour=FTR)) + geom_point()
scatter + geom_abline()
scatter
