library(dplyr)
library(ggplot2)
library(lubridate)

setwd("C:/Users/Devin Simmons/Desktop/classes/HONR238V/data")

june_trips <- read.csv("201506-citibike-tripdata.csv")

june_trips$starttime <- strptime(june_trips$starttime, format = '%m/%d/%Y %H:%M')
june_trips$stoptime <- strptime(june_trips$stoptime, format = '%m/%d/%Y %H:%M')
#not sure if this is necessary but it works
june_trips$starttime <- as.POSIXct(june_trips$starttime)
june_trips$stoptime <- as.POSIXct(june_trips$stoptime)

#truncate date to be just the day, this will be used to group observations and
#compare to weather events
june_trips$day <- format(as.Date(june_trips$starttime, format='%m-%d-%Y %H:%M'),"%m-%d-%Y")


#lubridate makes it easier to truncate to the hour
june_trips$hour <- round_date(ymd_hms(june_trips$starttime), 'hour')
#convert back to POSIXct so dplyr can work with it
june_trips$hour <- as.POSIXct(june_trips$hour)

#group trips by hour so it can be compared with hourly weather measurements
june_hourly_trips <- june_trips %>% group_by(june_trips$hour) %>% summarize(n())
#same for days
june_daily_trips <- june_trips %>% group_by(june_trips$day) %>% summarize(n())

#rename columns
names(june_hourly_trips) <- c('Hour', 'trips_taken')

# wind speed data
windSpeed <- read.csv("wind_speed.csv")

windSpeed$datetime <- ymd_hms(windSpeed$datetime)
windSpeed <- na.omit(windSpeed)
windSpeed <- subset(windSpeed,datetime>="2015-01-01" & datetime<="2016-06-01")
windSpeed_NYC <- select(windSpeed,datetime,New.York)
windSpeed_NYC$New.York <- windSpeed_NYC$New.York*2.237
colnames(windSpeed_NYC) <- c("Date","Wind Speed (mph)")

#join trips by hour to windspeed by hour based on time of observation
wind_vs_trips <- inner_join(june_hourly_trips, windSpeed_NYC, by = c('Hour' = 'Date'))

ggplot(data = wind_vs_trips, aes(x = wind_vs_trips$`Wind Speed (mph)`, y = wind_vs_trips$trips_taken)) + geom_point(color = 'blue') + labs(x = 'Hourly Wind Speed', y = 'Trips taken in an hour', title = 'June 2015 Citibike Trips Compared to Wind Speed') + geom_smooth(method = "lm", se = FALSE)
