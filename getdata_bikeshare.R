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




