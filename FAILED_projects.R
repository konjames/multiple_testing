# data$rounded.time <- round_date(as.POSIXct(data$Start.date, tz = "UTC"), "hour")
# data$Member.type.d <- as.numeric(data$Member.type) - 1 #Members are coded as 1
# data$garbage <- 1
# 
# temp <- aggregate(data$garbage, by=list(Category=data$rounded.time), FUN=sum)
# Lasso_data <- Lasso_data[Lasso_data$Date <= temp[dim(temp)[1],"Category"],]
# Lasso_data$Members <- aggregate(data$Member.type.d, by=list(Category=data$rounded.time), FUN=sum)
# 
# Lasso_data[1:(length(dates_lasso) -1), c("WND", "DEW", "VIS", "TMP")] =
#   weather[1:(length(dates_lasso) -1), c("WND", "DEW", "VIS", "TMP")]


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

for (station in stations) {
  popular_stations[popular_stations$Station == station,"Fall"] = 
    sum(data$Start.station.number == station & data$Season == "Fall")
  popular_stations[popular_stations$Station == station,"Winter"] = 
    sum(data$Start.station.number == station & data$Season == "Winter")
  popular_stations[popular_stations$Station == station,"Spring"] = 
    sum(data$Start.station.number == station & data$Season == "Spring")
  popular_stations[popular_stations$Station == station,"Summer"] = 
    sum(data$Start.station.number == station & data$Season == "Summer")
}
popular_stations$Total <- popular_stations$Fall + popular_stations$Winter + popular_stations$Spring + popular_stations$Summer



## Download weather data of Washing Reagan Airport 2011
## OLD VERSION FROM NCDC CDO 
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
  }
}

weather$DATE = round_date(as.POSIXct(weather$DATE, tz = "UTC"), "hour")
