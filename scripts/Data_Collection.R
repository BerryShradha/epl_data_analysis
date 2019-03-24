#################################################################################
###############################  DATA COLLECTION ################################

# 1. Use Darksky API to fetch weather data based on match days and home location
# 2. Make script generic

#################################################################################

#Install R library to access darksky API
#devtools::install_github("hrbrmstr/darksky", force = TRUE)

#Load libraries
library(darksky)
library(plyr)
library(dplyr)

#Call to API requires a secret key, provided by website on registration.
xd <- get_forecast_for(-33.86659241, 151.2081909, "2014-10-01T02:00:00", add_headers=TRUE)

#Read CSV with date, Lat, Long information
locations <- read.csv("./Weather/Dates_location_mapping.csv", header = T)

#Create a blank data frame to accumulate API results from each iteration
result = data.frame()

#Correcting first column's name to 'Id' because it is somehow getting a junk character in its name
colnames(locations)[1] <- "Id"

#Loop through each row of CSV
for(i in 1:length(locations$Id)){
  formatted_date <- paste(locations$Date[i], 'T13:00:00', sep = '') #Append a hardcoded time of 13:00 to match date format for API
  forcast <- get_forecast_for(locations$Latitiude[i], locations$Longitude[i], formatted_date) #API call using R library darksky
  forcast <- as.data.frame(forcast)
  forcast <- forcast[2, grepl("daily", names(forcast))] #Store only those columns that have name starting with 'daily'. Don't need hourly data 
  forcast <- mutate(id = locations$Id[i], location = locations$Location[i], home.team = locations$Home.Team[i], 
                    latitude = locations$Latitiude[i], longitude = locations$Longitude[i], forcast) #Append important mapping columns from csv to the result
  result <-  rbind.fill(result, forcast) #Bind result from each iteration
}

#Save result in a csv
write.table(result, file = "Weather.csv",row.names=FALSE, na="", sep=",")
