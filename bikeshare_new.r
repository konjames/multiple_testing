## LIBRARIES
library("lubridate")
library("readr")
library("forecast")


data <- read_csv("Multiple_testing_proj/2011-capitalbikeshare-tripdata.csv")
Capital_Bike_Share_Locations <- read_csv("Multiple_testing_proj/Capital_Bike_Share_Locations.csv")

data = NULL

for(i in 2011:2011) {
  file = paste0('https://s3.amazonaws.com/capitalbikeshare-data/',i,'-capitalbikeshare-tripdata.zip')
  download.file(file,destfile='bikedata.zip')
  unzip('bikedata.zip')
  data = rbind(data,read.csv(paste0(i,'-capitalbikeshare-tripdata.csv')))
}

n = dim(data)[1]

starttime = as.numeric(strsplit(toString(data[,2]),split='[-:, ]')[[1]][-7*(1:n)]) # start time of ride #i
dim(starttime) = c(6,n); starttime = t(starttime) # row i = year/month/date/hour/minute/second for ride #i
duration = data[,1] # duration of the ride in seconds
station_start = data[,4] # station ID where the bike was checked out
station_end = data[,6] # station ID where the bike was returned
bikenum =  as.numeric((strsplit(toString(data[,8]),'[?wW, ]')[[1]][3*(1:n)-1])) # some are NA, the data is messy for this one
member = (data[,9]=='Member') # member (1) or nonmember (0)
endtime = as.numeric(strsplit(toString(data[,3]),split='[-:, ]')[[1]][-7*(1:n)]) 
dim(endtime) = c(6,n); endtime = t(endtime)

stations = NULL # stations[i,1] = station ID for the i-th station, stations[i,2] = station location for the i-th station
for(i in unique(c(station_start,station_end))){
  if(any(data[,4]==i)){
    ind = min(which(data[,4]==i))
    location = toString(data[ind,5])
  }else{
    ind = min(which(data[,6]==i))
    location = toString(data[ind,7])
  }
  stations = rbind(stations,c(i,location))
}


## FUNCTIONS
## Finds Hourly demand
hourly_demand = function(month, day, hour){
  count = 0 
  for (i in 1:n){
    if (starttime[i,2] == month && starttime[i,3] == day){
      if (starttime[i,4]>=hour && endtime[i,4]<=hour){
        count = count+1
      }
    }
  }
  return (count)
}
## Public Holiday Function (Returns 1 if public holiday, Input is date)
Holiday_check = function(date) {
  date = as.Date(date) 
  dates <- c("2017-1-1", "2017-1-2", "2017-1-16", "2017-1-20", 
             "2017-2-20", "2017-3-17", "2017-5-29", "2017-7-4", 
             "2017-9-4", "2017-10-9", "2017-11-10", "2017-11-23", 
             "2015-12-25") #HOLIDAYS
  dates <- as.Date(dates)
  dates <- (dates == date)
  
  if(sum(dates) >= 1) {
    return(1)
  } else {
    return(0)
  }
}


## Download weather data of Washing Reagan Airport 2017 (WRONG YEAR BUT WILL CHANGE TOMORROW)
weather <- read_csv("Desktop/reaganairport.csv")

## CLEAN THE Weather data
weather <- data.frame(weather)
weather$Day <- as.Date(weather$DATE)

drops <- c("STATION","CALLSIGN", "SOURCE", "REPORT_TYPE", "QUALITY_CONTROL", 
           "QUALITY_CONTROL_1", "REPORT_TYPE_1", "SOURCE_1")

weather <- weather[ , !(names(weather) %in% drops)]

dates = seq(as.POSIXct("2011-01-01 00:00:00", tz = "UTC"), as.POSIXct("2012-01-01 22:00:00", tz = "UTC"), by="hour")

for (i in 1:(length(dates) - 1)) {
  date1 <- dates[i]
  date2 <- dates[i+1]
  int <- interval(date1, date2)
  a = weather[weather$DATE %within% int, "DATE"]
  minimum = c(a)[1]
  if(!is.infinite(minimum)) {
    weather <- weather[ !((weather$DATE %within% int) & 
                            weather$DATE != minimum),] 
  }
}

weather$DATE = round_date(as.POSIXct(weather$DATE, tz = "UTC"), "hour")


start_date = round_date(as.POSIXct(data[1, "Start.date"], tz = "UTC"), "hour")
end_date = round_date(as.POSIXct(data[dim(data)[1], "End.date"], tz = "UTC"), "hour")

dates_lasso = seq(start_date, end_date, by="hour")
dates_lasso = as.POSIXct(dates)
Lasso_data = data.frame(matrix(ncol = 6, nrow = length(dates)))
colnames(Lasso_data) <- c("Date", "Rides", "WND", "DEW", "VIS", "TMP")
Lasso_data[, 1] = dates_lasso

data$Start.date <- as.POSIXct(data$Start.date, tz = "UTC")
for(i in 1:(length(dates_lasso) -1)) {
  date1 <- as.POSIXct(dates_lasso[i])
  date2 <- as.POSIXct(dates_lasso[i+1])
  int <- interval(date1, date2)
  Lasso_data[Lasso_data$Date == date1, "Rides"] = dim(data[data$Start.date %within% int,])[1]
}
Lasso_data[1:(length(dates_lasso) -1), c("WND", "DEW", "VIS", "TMP")] = weather[1:(length(dates_lasso) -1), c("WND", "DEW", "VIS", "TMP")]

#FOR HOLIDAY INDICATOR
Lasso_data[,"Holiday"] <- 0 
for (i in 1:(length(dates_lasso) -1)) {
  Lasso_data[i,"Holiday"] <- Holiday_check(as.POSIXct(dates_lasso[i]))
}

## FOR DAY INDICATOR
Lasso_data[,"Day"] <- 0 
for (i in 1:(length(dates_lasso) -1)) {
  day <- as.POSIXct(Lasso_data[i,Date])
  Lasso_data[i,"Day"] <- wday(Lasso_data[1:(length(dates_lasso) -1), "Date"], label = TRUE)
}

temp<- wday(ymd_hms(Lasso_data[1:(length(dates_lasso) -1), "Date"]), label = TRUE)
Lasso_data[1:(length(dates_lasso) -1), "Day"] <- wday(temp, label = TRUE)

## FOR DAY INDICATOR
temp <- wday(Lasso_data[, "Date"], label = TRUE)
Lasso_data[, "Day"] <- temp





## Selective Inference for ranking
stations <- levels(as.factor(data2$`Start station number`))
popular_stations <- data.frame(matrix(nrow = length(stations), ncol = 2))
colnames(popular_stations) <- c("Station", "End.Station")
popular_stations$Station <- stations

for(station in stations) {
  temp_data <- data2[data2$`Start station number` == station,]
  end_dests <- stations[stations != station]
  
  route_counter <- data.frame(matrix(nrow = length(end_dests), ncol = 2))
  colnames(route_counter) <- c("End.Station", "Count")
  route_counter$End.Station <- end_dests
  for(end_dest in end_dests) {
    route_counter[route_counter$End.Station == end_dest, "Count"] <- dim(temp_data[temp_data$`End station number` == end_dest,])[1]
  }
  winner <- route_counter[route_counter$Count == max(route_counter$Count), "End.Station"]
  second_place <- sort(route_counter$Count, decreasing = TRUE)[2]
  total <- sum(route_counter$Count)
  subset_total <- total - sum(sort(route_counter$Count, decreasing = TRUE)[c(-1,-2)])
  
  
}

