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

