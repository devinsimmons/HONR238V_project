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
names(june_daily_trips) <- c('day', 'trips_taken')

june_daily_trips$day <- strptime(june_daily_trips$day, format = '%m-%d-%Y')
june_daily_trips$day <- as.POSIXct(june_daily_trips$day)


# wind speed data
windSpeed <- read.csv("wind_speed.csv")

windSpeed$datetime <- ymd_hms(windSpeed$datetime)
View(windSpeed)
windSpeed <- na.omit(windSpeed)
windSpeed <- subset(windSpeed,datetime>="2015-01-01" & datetime<="2016-06-01")
windSpeed_NYC <- select(windSpeed,datetime,New.York)
windSpeed_NYC$New.York <- windSpeed_NYC$New.York*2.237
colnames(windSpeed_NYC) <- c("Date","Wind Speed (mph)")

# temperature data for NYC
temperature <- read.csv('temperature.csv')
temperature$datetime <- strptime(temperature$datetime, format = '%m/%d/%Y %H:%M')
temperature$datetime <- as.POSIXct(temperature$datetime)
temperature <- na.omit(temperature)
temperature <- subset(temperature,datetime>="2015-01-01" & datetime<="2016-06-01")
temperature_NYC <- select(temperature,datetime,New.York)
temperature_NYC$New.York <- 9/5*(temperature_NYC$New.York-273)+32
colnames(temperature_NYC) <- c("Datetime","deg_fahrenheit")
#parse out the date of each observation
temperature_NYC$date <- round_date(temperature_NYC$Datetime, 'day')



#determines the average temperature for each day
avg_daily_temp <- temperature_NYC %>% group_by(temperature_NYC$date) %>% summarize(mean_daily_temp = mean(deg_fahrenheit))
colnames(avg_daily_temp) <- c('day', 'temp_f')

#join trips by hour to windspeed by hour based on time of observation
wind_vs_trips <- inner_join(june_hourly_trips, windSpeed_NYC, by = c('Hour' = 'Date'))
#join trips by day to temperature by day
temp_vs_trips <- inner_join(june_daily_trips, avg_daily_temp, by = c('day' = 'day'))
View(temp_vs_trips)

#linear regression statistics for temp vs trips
lin_reg_temp <- lm(trips_taken ~ temp_f, data = temp_vs_trips)
summary(lin_reg_temp)


ggplot(data = wind_vs_trips, aes(x = wind_vs_trips$`Wind Speed (mph)`, y = wind_vs_trips$trips_taken)) + geom_point(color = 'blue') + labs(x = 'Hourly Wind Speed', y = 'Trips taken in an hour', title = 'June 2015 Citibike Trips Compared to Wind Speed') + stat_smooth(method = "lm", col = 'black')

temp_corr <- ggplot(data = temp_vs_trips, aes(x = temp_vs_trips$temp_f, y = temp_vs_trips$trips_taken)) + geom_point(color = 'blue') + labs(x = 'Mean daily temperature, degrees Fahrenheit', y = 'Trips taken in a day', title = 'June 2015 Citibike Daily Ridership Compared to Temperature') + stat_smooth(method = "lm", col = 'black') + annotate("text", x = 60, y = 10000, label = "y = 315X + 8612.2, R2 = 0.0201")

ggsave(filename="temp_and_bike_ridership.png", plot=temp_corr)