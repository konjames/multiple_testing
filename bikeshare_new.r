## LIBRARIES
library("lubridate")
library("readr")
library("forecast")

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

write.csv(data, file = "cleaned_data.csv")

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

## This function rounds times down to the hour. 
round_down = function(times) {
  secs <- as.POSIXlt(times, tz = "UTC")$sec
  mins <- as.POSIXlt(times, tz = "UTC")$min
  return(as.POSIXlt(times, tz = "UTC") - (mins*60) - secs)
}


## Download weather data of Washing Reagan Airport 2017 (WRONG YEAR BUT WILL CHANGE TOMORROW)
weather <- read_csv("reaganairport.csv")

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
    #weather <- weather[ -which((weather$DATE %within% int) & weather$DATE != minimum),] 
  }
}
weather$DATE = round_down(weather$DATE)

write.csv(weather, file = "weather_cleaned.csv")

start_date = round_date(as.POSIXct(data[1, "Start.date"], tz = "UTC"), "hour")
end_date = round_date(as.POSIXct("2012-1-1 23:00:00", tz = "UTC"), "hour")
weather <- weather[weather$DATE <= as.POSIXct("2011-12-31 23:00:00", tz = "UTC"),] # USE IF NOT WORKING weather <- weather[weather$DATE < as.POSIXct("2012-01-01", tz = "UTC"),]



dates_lasso = seq(start_date, end_date, by="hour")
dates_lasso = as.POSIXct(dates)
Lasso_data = data.frame(matrix(ncol = 7, nrow = length(dates)))
colnames(Lasso_data) <- c("Date", "Rides", "WND", "DEW", "VIS", "TMP", "Members")
Lasso_data[, 1] = dates_lasso

data$Start.date <- as.POSIXct(data$Start.date, tz = "UTC")
data$TMP <- 0 
for(i in 1:(length(dates_lasso) -1)) {
  date1 <- as.POSIXct(dates_lasso[i])
  date2 <- as.POSIXct(dates_lasso[i+1])
  int <- interval(date1, date2)
  #Lasso_data[Lasso_data$Date == date1, "Rides"] = dim(data[data$Start.date %within% int,])[1]
  #Lasso_data[Lasso_data$Date == date1, "Members"] = dim(data[data$Start.date %within% int & data$Member.type == "Member",])[1]
  Lasso_data[Lasso_data$Date == date1, "Members"] = sum(data$Start.date %within% int & data$Member.type == "Member")
  Lasso_data[Lasso_data$Date == date1, "Rides"] = sum(data$Start.date %within% int)
  data[data$Start.date %within% int, "TMP"] <- weather[weather$DATE == date1, "TMP"]
}
write.csv(data, file = "cleaned_data.csv")

#FOR HOLIDAY INDICATOR
Lasso_data[,"Holiday"] <- 0 
for (i in 1:(length(dates_lasso) -1)) {
  Lasso_data[i,"Holiday"] <- Holiday_check(as.POSIXct(dates_lasso[i]))
}

## FOR DAY INDICATOR
Lasso_data$Day <- 0 
Lasso_data[, "Day"] <- wday(Lasso_data[, "Date"], label = TRUE)

Lasso_data$Weekend <- 0 
Lasso_data[Lasso_data$Day == "Sat", "Weekend"] <- 1
Lasso_data[Lasso_data$Day == "Sun", "Weekend"] <- 1

Lasso_data[, c("WND", "DEW", "VIS", "TMP")] = weather[, c("WND", "DEW", "VIS", "TMP")] 

write.csv(Lasso_data, file = "Lasso_data.csv")


## QUESTION: IS THERE A CHANGE IN THE DEMAND FOR A STATION BETWEEN SEASONS
## TEST: USING DIFFERENT OF MEANS NORMAL DISTRIBUTION: USE PERMUTATION TEST 
data$Season <- "Fall"
data$Start.date <- as.POSIXct(data$Start.date)
winter <- interval(as.POSIXct("2011-01-01", tz = "UTC"), as.POSIXct("2011-03-20", tz = "UTC")) # Winter dates
data[data$Start.date %within% winter, "Season"] <- "Winter"

winter <- interval(as.POSIXct("2011-12-21 01:00:00", tz = "UTC"), as.POSIXct("2011-12-31 23:00:00", tz = "UTC")) # Winter dates
data[data$Start.date %within% winter, "Season"] <- "Winter"

spring <- interval(as.POSIXct("2011-03-20 01:00:00", tz = "UTC"), as.POSIXct("2011-06-20", tz = "UTC")) # Spring dates
data[data$Start.date %within% spring, "Season"] <- "Spring"

summer <- interval(as.POSIXct("2011-06-20 01:00:00", tz = "UTC"), as.POSIXct("2011-09-22", tz = "UTC")) # Fall dates
data[data$Start.date %within% summer, "Season"] <- "Summer"




stations <- levels(as.factor(data$Start.station.number))
popular_stations <- data.frame(matrix(nrow = length(stations), ncol = 6))
colnames(popular_stations) <- c("Station", "Fall", "Winter", "Spring", "Summer", "Total")
popular_stations$Station <- stations


T1 = function(X, y){
  mean_treatment <- mean(X[y])
  mean_NO_treatment <- mean(X[!y])
  return(mean_treatment - mean_NO_treatment)
}

T2 = function(X, y){
  return(cor(X,y))
}

# This function runs the permutation test using tstatistics 1
permutation_test <- function(X,y) {
  nperm = length(X) * .5
  T_realdata = T2(X,y)
  T_perm = rep(0,nperm)
  for(i in 1:nperm){
    perm = sample(dim(X)[1],dim(X)[1]) # scramble the numbers 1 through 560 to scramble the patient labels
    T_perm[i] = T1(X[perm],y) # this shuffles the rows of X
  }
  pval = (1 + sum(T_perm>=T_realdata)) / (1 + nperm)
  return(pval)
}


tempars <- seq(40,90)
for (temp in tempars) {
  p.names <- c(p.names, paste("p.value", temp, sep="."))
}

Rdiff_mean <- data.frame(matrix(nrow = length(tempars), ncol = 2))
colnames(Rdiff_mean) <- c("temp", "p.value")
Rdiff_mean$temp <- tempars


## NAIVE VERSION WITHOUT DETRENDING OR CONTROLLING FOR ANYTHING and using sums
for (temp in tempars){
  data_treat <- data[data$TMP >= tempars, c("Start.station.number", "Duration")]
  data_control <- data[data$TMP < tempars,]
  stations_diff <- data.frame(matrix(nrow = length(stations), ncol = 4))
  colnames(stations_diff) <- c("Station", "Control", "Treatment", "Diff")
  stations_diff$Station <- stations
  for (station in stations) {
    stations_diff[stations_diff$Station == station, "Control"]  = 
      sum(data_control$Start.station.number == station)
    stations_diff[stations_diff$Station == station, "Treatment"]  = 
      sum(data_treat$Start.station.number == station)
  }
  stations_diff$Diff <- stations_diff$Control - stations_diff$Treatment
  n_stat <- length(stations)
  var_stat <- var(stations_diff$Control)/n_stat + var(stations_diff$Treatment)/n_stat
  t.stat <- (mean(stations_diff$Diff) - 0) / sqrt(var_stat)
  Rdiff_mean[Rdiff_mean$temp == temp, "p.value"] <- 1 - pnorm(t.stat)
}



## USING DURATION AS A MEASUREMENT OF BIKE USAGE
Rduration <- data.frame(matrix(nrow = length(stations), ncol = (1+length(p.names))))
colnames(Rduration) <- c("station", p.names)
Rduration$stations <- stations

for (temp in tempars){
  data_treat <- data[data$TMP >= tempars, c("Start.station.number", "Duration")]
  data_control <- data[data$TMP < tempars, c("Start.station.number", "Duration")]

  for (station in stations) {
    X_1 <- data_treat[data_treat$Start.station.number == station, "Duration"]
    X_2 <- data_treat[data_treat$Start.station.number == station, "Duration"]
    y <- rep()
  }
  stations_diff$Diff <- stations_diff$Control - stations_diff$Treatment
  n_stat <- length(stations)
  var_stat <- var(stations_diff$Control)/n_stat + var(stations_diff$Treatment)/n_stat
  t.stat <- (mean(stations_diff$Diff) - 0) / sqrt(var_stat)
  p_results_perm[p_results_perm$temp == temp, p.value] <- 1 - pnorm(t.stat)
}

