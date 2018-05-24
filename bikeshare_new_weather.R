## LIBRARIES
library("lubridate")
library("readr")
install.packages("forecast", dependencies = TRUE)
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


################################# FUNCTIONS ######################################################
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

# Is the implementation of within without within pacakage or lubridate pacakge. 
hour_int <- function(times, date1, date2) {
  trues = (times >= date1) & (times <= date2) 
  return(trues)
}

# Statistics 1 - Difference of Means
# X = Some continous vector, y = treatment vector
T1 = function(X, y){
  mean_treatment <- mean(X[y])
  mean_NO_treatment <- mean(X[!y])
  return(mean_treatment - mean_NO_treatment)
}

# Statistics 2 - Correlation
# X = Some continous vector, y = treatment vector
T2 = function(X, y){
  return(cor(X,y))
}

# Statistics 3 - Difference of Means Test
# X = Some continous vector, y = treatment vector
T3 = function(X, y){
  treat <- X[y]
  no_treat <- X[!y]
  mean_treatment <- mean(treat)
  mean_NO_treatment <- mean(no_treat)
  var_stat <- var(treat)/treat + var(no_treat)/length(no_treat)
  t.stat <- (mean_treatment - mean_NO_treatment - 0) / sqrt(var_stat)
  return(2*pnorm(-abs(t.stat)))
}

T4 = function(X, y) {
  
}

# This function runs the permutation test using T2
permutation_test <- function(X,y) {
  nperm = length(X) * .5
  T_realdata = T2(X,y)
  T_perm = rep(0,nperm)
  for(i in 1:nperm){
    perm = sample(dim(X)[1],dim(X)[1]) # scramble the numbers 1 through 560 to scramble the patient labels
    T_perm[i] = T2(X[perm],y) # this shuffles the rows of X
  }
  pval = (1 + sum(T_perm>=T_realdata)) / (1 + nperm)
  return(pval)
}



### SOURCE: http://www.frontierweather.com/historicaldatasubscribers_hourly.html
weather <- data.frame(read.csv("Desktop/weather.csv"))
weather$Date <- seq(as.POSIXct("2009-1-1 00:00:00", tz = "UTC"), as.POSIXct("2017-12-31 23:00:00", tz = "UTC"), by="hour")
weather <- weather[, !(colnames(weather) %in% c("Site", "Hour", "Source"))]
#weather[hour_int(weather$Date, as.POSIXct("2011-01-01", tz = "UTC"), as.POSIXct("2012-01-01", tz = "UTC")))]
write.csv(weather, file = "weather_cleaned.csv")

# CAN CHANGE IF YOU WANT TO USE DIFFERENT INTERVALS
int = interval(as.POSIXct("2011-01-01", tz = "UTC"), as.POSIXct("2012-01-01", tz = "UTC"))
weather <- weather[weather$Date %within% int,]

start_date = round_date(as.POSIXct(data[1, "Start.date"], tz = "UTC"), "hour")
end_date = round_date(as.POSIXct(data[dim(data)[1], "Start.date"], tz = "UTC"), "hour")

dates_lasso = seq(start_date, end_date, by="hour")
dates_lasso = as.POSIXct(dates_lasso)
Lasso_data = data.frame(matrix(ncol = 7, nrow = length(dates_lasso)))
colnames(Lasso_data) <- c("Date", "Rides", "Temperature","Dewpoint","RH","WindDir","Windspeed","CldFrac","MSLP","Weather","Precip", "Members")
Lasso_data[, 1] = dates_lasso

data$Start.date <- as.POSIXct(data$Start.date, tz = "UTC")
for(i in 1:(length(dates_lasso) -1)) {
  date1 <- as.POSIXct(dates_lasso[i])
  date2 <- as.POSIXct(dates_lasso[i+1])
  int <- interval(date1, date2)
  Lasso_data[Lasso_data$Date == date1, "Members"] = sum(data$Start.date %within% int & data$Member.type == "Member")
  Lasso_data[Lasso_data$Date == date1, "Rides"] = sum(data$Start.date %within% int)

  #Lasso_data[Lasso_data$Date == date1, "Members"] = sum(hour_int(data$Date, date1, date2) & data$Member.type == "Member")
  #Lasso_data[Lasso_data$Date == date1, "Rides"] = sum(hour_int(data$Date, date1, date2))
}

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

Lasso_data[, c("Temperature","Dewpoint","RH","WindDir","Windspeed","CldFrac","MSLP","Weather","Precip")] <- 
  weather[, c("Temperature","Dewpoint","RH","WindDir","Windspeed","CldFrac","MSLP","Weather","Precip")]


write.csv(Lasso_data, file = "Lasso_data.csv")

# inserts weather into the dataframe. 
data$TMP <- 0 
for(i in 1:(length(dates_lasso) -1)) {
  date1 <- as.POSIXct(dates_lasso[i])
  date2 <- as.POSIXct(dates_lasso[i+1])
  int <- interval(date1, date2)
  data[data$Start.date %within% int, "TMP"] = weather[weather$Date == date1, "Temperature"]
}

data$Day <- 0 
data$Day <- wday(data$Date, label = TRUE)

data$Weekend <- 0 
data[data$Day == "Sat", "Weekend"] <- 1
data[data$Day == "Sun", "Weekend"] <- 1


## QUESTION: IS THERE A CHANGE IN THE DEMAND FOR A STATION BETWEEN SEASONS
## TEST: USING DIFFERENT OF MEANS NORMAL DISTRIBUTION: USE PERMUTATION TEST 
data$Season <- "Fall"
data$Start.date <- as.POSIXct(data$Start.date)
winter <- interval(as.POSIXct("2011-01-01", tz = "UTC"), as.POSIXct("2011-03-20", tz = "UTC")) # Winter dates
data[data$Start.date %within% winter, "Season"] <- "Winter"
# data[hour_int(data$Start.date, as.POSIXct("2011-01-01", tz = "UTC"), as.POSIXct("2011-03-20", tz = "UTC")), "Season"] <- "Winter"

winter <- interval(as.POSIXct("2011-12-21 01:00:00", tz = "UTC"), as.POSIXct("2011-12-31 23:00:00", tz = "UTC")) # Winter dates
data[data$Start.date %within% winter, "Season"] <- "Winter"
# data[hour_int(data$Start.date, as.POSIXct("2011-12-21 01:00:00", tz = "UTC"), as.POSIXct("2011-12-31 23:00:00", tz = "UTC")), "Season"] <- "Winter"

spring <- interval(as.POSIXct("2011-03-20 01:00:00", tz = "UTC"), as.POSIXct("2011-06-20", tz = "UTC")) # Spring dates
data[data$Start.date %within% spring, "Season"] <- "Spring"
# data[hour_int(data$Start.date, as.POSIXct("2011-03-20 01:00:00", tz = "UTC"), as.POSIXct("2011-06-20", tz = "UTC")), "Season"] <- "Spring"

summer <- interval(as.POSIXct("2011-06-20 01:00:00", tz = "UTC"), as.POSIXct("2011-09-22", tz = "UTC")) # Fall dates
data[data$Start.date %within% summer, "Season"] <- "Summer"
# data[hour_int(data$Start.date, as.POSIXct("2011-06-20 01:00:00", tz = "UTC"), as.POSIXct("2011-09-22", tz = "UTC")), "Season"] <- "Summer"



stations <- levels(as.factor(data$Start.station.number))
popular_stations <- data.frame(matrix(nrow = length(stations), ncol = 6))
colnames(popular_stations) <- c("Station", "Fall", "Winter", "Spring", "Summer", "Total")
popular_stations$Station <- stations


temperatures <- seq(40,90, by  = 5)
p.names = c()
for (temp in temperatures) {
  p.names <- c(p.names, paste("p.value", temp, sep="."))
}


Rdiff_mean <- data.frame(matrix(nrow = length(temperatures), ncol = 2))
colnames(Rdiff_mean) <- c("temp", "p.value")
Rdiff_mean$temp <- temperatures


## NAIVE VERSION WITHOUT DETRENDING OR CONTROLLING FOR ANYTHING and using difference of means test
for (temp in temperatures){
  data_treat <- data[data$TMP >= temp, "Start.station.number"]
  data_control <- data[data$TMP < temp, "Start.station.number"]
  stations_diff <- data.frame(matrix(nrow = length(stations), ncol = 4))
  colnames(stations_diff) <- c("Station", "Control", "Treatment", "Diff")
  stations_diff$Station <- stations
  for (station in stations) {
    stations_diff[stations_diff$Station == station, "Control"]  = 
      sum(data_control == station)
    stations_diff[stations_diff$Station == station, "Treatment"]  = 
      sum(data_treat == station)
  }
  stations_diff$Diff <- stations_diff$Treatment - stations_diff$Control
  n_stat <- length(stations)
  var_stat <- var(stations_diff$Control)/n_stat + var(stations_diff$Treatment)/n_stat
  t.stat <- (mean(stations_diff$Diff) - 0) / sqrt(var_stat)
  Rdiff_mean[Rdiff_mean$temp == temp, "p.value"] <- 2*pnorm(-abs(t.stat))
}


## USING DURATION AS A MEASUREMENT OF BIKE USAGE - NAIVE (for stations)
Rduration <- data.frame(matrix(nrow = length(stations), ncol = (1+length(temperatures))))
colnames(Rduration) <- c("Station", p.names)
Rduration$Station <- stations

for (temp in temperatures){
  data_treat <- data[data$TMP >= temp, c("Start.station.number", "Duration")]
  data_control <- data[data$TMP < temp, c("Start.station.number", "Duration")]

  for (station in stations) {
    X_1 <- data_treat[data_treat$Start.station.number == station, "Duration"]
    X_2 <- data_control[data_control$Start.station.number == station, "Duration"]
    y <- c(rep(1, length(X_1)), rep(0, length(X_2)))
    X <- c(X_1, X_2)
    Rduration[Rduration$Station == station, paste("p.value", temp, sep=".")] = permutation_test(X, y)
  }
}


## USING DURATION AS A MEASUREMENT OF BIKE USAGE - NAIVE (for everything)
naive_results <- data.frame(matrix(nrow = length(temperatures), ncol = 2))
colnames(naive_results) <- c("Temperature", "p.value")
naive_results$Temperature <- temperatures
trunc_data <- data[, c("TMP", "Duration")]
nperm = 100
for (temp in temperatures) {
  X_1 <- trunc_data[trunc_data$TMP >= temp, "Duration"]
  X_2 <- trunc_data[trunc_data$TMP < temp, "Duration"]
  y <- c(rep(1, length(X_1)), rep(0, length(X_2)))
  X <- c(X_1, X_2)
  T_realdata = T2(X,y)
  T_perm = rep(0,nperm)
  for(i in 1:nperm){
    perms <- sample(dim(trunc_data)[1], dim(trunc_data)[1])
    y <- y[perms]
    T_perm[i] <- T2(X, y)
  }
  pval = (1 + sum(T_perm>=T_realdata)) / (1 + nperm)
  naive_results[naive_results$Temperature == temp, "p.value"] = pval
}

## USING DURATION AS A MEASUREMENT OF BIKE USAGE - PERMUTING BY SEASON
seasonal_perm_results <- data.frame(matrix(nrow = length(temperatures), ncol = 2))
colnames(seasonal_perm_results) <- c("Temperature", "p.value")
seasonal_perm_results$Temperature <- temperatures
trunc_data <- data[, c("TMP", "Duration", "Season")]
nperm = 100
for (temp in temperatures) {
  X_1 <- trunc_data[trunc_data$TMP >= temp, "Duration"]
  X_2 <- trunc_data[trunc_data$TMP < temp, "Duration"]
  treatment <- data.frame(c(rep(1, length(X_1)), rep(0, length(X_2))), trunc_data$Season)
  colnames(treatment) <- c("y", "Season")
  X <- c(X_1, X_2)
  T_realdata = T2(X,treatment$y)
  T_perm = rep(0,nperm)
  for(i in 1:nperm) {
    garbage <- treatment
    for (season in c("Fall", "Winter", "Spring", "Summer")) {
      condition_vec <- (garbage$Season == season)
      perms <- sample(sum(condition_vec), sum(garbage$Season == season))
      garbage[condition_vec, "y"] <- (garbage[condition_vec, "y"][perms])
    }
    T_perm[i] <- T2(X, garbage$y)
  }
  pval = (1 + sum(T_perm>=T_realdata)) / (1 + nperm)
  seasonal_perm_results[seasonal_perm_results$Temperature == temp, "p.value"] = pval
}
