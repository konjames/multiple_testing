setwd("~/Desktop")
data = NULL

for(i in 2010:2011){ # the data goes up to 2017, but the files are extremely large from 2011 onwards - you can decide to just use a subset
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
# note that stations get added to the program over time

Capital_Bike_Share_Locations <- read_csv("~/Desktop/Capital_Bike_Share_Locations.csv")

data$Start_latitude = 0
data$Start_longitude = 0

data$end_latitude = 0 
data$end_longitude = 0

# METHOD 1
#for (i in 1:dim(data)[1]) {
#  data[i, "Start_latitude"] <- Capital_Bike_Share_Locations[Capital_Bike_Share_Locations$TERMINAL_NUMBER == data[i, "Start.station.number"],]$LATITUDE
#  data[i, "Start_longitude"] <- Capital_Bike_Share_Locations[Capital_Bike_Share_Locations$TERMINAL_NUMBER == data[i, "Start.station.number"],]$LONGITUDE
#  data[i, "end_latitude"] <- Capital_Bike_Share_Locations[Capital_Bike_Share_Locations$TERMINAL_NUMBER == data[i, "End.station.number"],]$LATITUDE
#  data[i, "end_longitude"] <- Capital_Bike_Share_Locations[Capital_Bike_Share_Locations$TERMINAL_NUMBER == data[i, "End.station.number"],]$LONGITUDE
#}


# METHOD 2
#for (station in Capital_Bike_Share_Locations$TERMINAL_NUMBER) {
#  if (sum(data$Start.station.number == station) > 0){
#    data[data$Start.station.number == station,]$LATITUDE <- Capital_Bike_Share_Locations[Capital_Bike_Share_Locations$TERMINAL_NUMBER == station,]$LATITUDE
#    data[data$Start.station.number == station,]$LONGITUDE <- Capital_Bike_Share_Locations[Capital_Bike_Share_Locations$TERMINAL_NUMBER == station,]$LONGITUDE
#  }
#  if (sum(data$End.station.number == station) > 0) {
#    data[data$End.station.number == station,]$LATITUDE <- Capital_Bike_Share_Locations[Capital_Bike_Share_Locations$TERMINAL_NUMBER == station,]$LATITUDE
#    data[data$End.station.number == station,]$LONGITUDE <- Capital_Bike_Share_Locations[Capital_Bike_Share_Locations$TERMINAL_NUMBER == station,]$LONGITUDE
#  }
#}


# If the data is already read in, use these commands
data = read.csv("~/Desktop/2010-capitalbikeshare-tripdata.csv")
#data = rbind(data,read.csv('~/Desktop/2011-capitalbikeshare-tripdata.csv'))
data[, c("Start.date.day", "Start.date.time")] <- t(data.frame(strsplit(as.character(data$Start.date), " ")))
data[, c("End.date.day", "End.date.time")] <- t(data.frame(strsplit(as.character(data$End.date), " ")))
data$Start.date.day <- as.Date(data$Start.date.day)
data$End.date.day <- as.Date(data$End.date.day)

sept <- data[as.Date("2010-9-01","%Y-%m-%d") <= data$Start.date.day & as.Date("2010-9-31","%Y-%m-%d") >= data$Start.date.day,]
oct <- data[as.Date("2010-10-01","%Y-%m-%d") <= data$Start.date.day & as.Date("2010-10-31","%Y-%m-%d") >= data$Start.date.day,]
nov <- data[as.Date("2010-11-01","%Y-%m-%d") <= data$Start.date.day & as.Date("2010-11-31","%Y-%m-%d") >= data$Start.date.day,]
dec <- data[as.Date("2010-12-01","%Y-%m-%d") <= data$Start.date.day & as.Date("2010-12-31","%Y-%m-%d") >= data$Start.date.day,]
