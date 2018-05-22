library("lubridate")

## Download weather data from Washing Reagan Airport. 
weather <- read_csv("~/Desktop/reaganairport.csv")
weather <- data.frame(weather)

weather$Day <- as.Date(weather$DATE)

drops <- c("STATION","CALLSIGN", "SOURCE", "REPORT_TYPE", "QUALITY_CONTROL", 
           "QUALITY_CONTROL_1", "REPORT_TYPE_1", "SOURCE_1")

weather <- weather[ , !(names(weather) %in% drops)]


dates = seq(as.POSIXct("2017-01-01 00:00:00", tz = "UTC"), as.POSIXct("2018-01-01 22:00:00", tz = "UTC"), by="hour")

for (i in 1:(length(dates) - 1)) {
  date1 <- dates[i]
  date2 <- dates[i+1]
  int <- interval(date1, date2)
  a = weather[weather$DATE %within% int, "Minute"]
  minimum = c(a)[1]
  if(!is.infinite(minimum)) {
    weather <- weather[ !((weather$DATE %within% int) & 
                                 weather$Minute != minimum),] 
  }
}


