library(dplyr)
library(ggplot2)
library(lubridate)

setwd("C:/Users/Devin Simmons/Desktop/classes/HONR238V/data")

june_trips <- read.csv("201506-citibike-tripdata.csv")
jan_trips <- read.csv("201501-citibike-tripdata.csv")
feb_trips <- read.csv("201502-citibike-tripdata.csv")
mar_trips <- read.csv("201503-citibike-tripdata.csv")
apr_trips <- read.csv("201504-citibike-tripdata.csv")
may_trips <- read.csv("201505-citibike-tripdata.csv")

trips_2015 <- rbind(jan_trips, feb_trips, mar_trips, apr_trips, may_trips, trips_2015)

trips_2015$starttime <- strptime(trips_2015$starttime, format = '%m/%d/%Y %H:%M')
trips_2015$stoptime <- strptime(trips_2015$stoptime, format = '%m/%d/%Y %H:%M')
#not sure if this is necessary but it works
trips_2015$starttime <- as.POSIXct(trips_2015$starttime)
trips_2015$stoptime <- as.POSIXct(trips_2015$stoptime)

#truncate date to be just the day, this will be used to group observations and
#compare to weather events
trips_2015$day <- format(as.Date(trips_2015$starttime, format='%m-%d-%Y %H:%M'),"%m-%d-%Y")

#lubridate makes it easier to truncate to the hour
trips_2015$hour <- round_date(ymd_hms(trips_2015$starttime), 'hour')
#convert back to POSIXct so dplyr can work with it
trips_2015$hour <- as.POSIXct(trips_2015$hour)

#group trips by hour so it can be compared with hourly weather measurements
hourly_trips <- trips_2015 %>% group_by(trips_2015$hour) %>% summarize(n())
#same for days
daily_trips <- trips_2015 %>% group_by(trips_2015$day) %>% summarize(n())

#rename columns
names(hourly_trips) <- c('Hour', 'trips_taken')
names(daily_trips) <- c('day', 'trips_taken')

daily_trips$day <- strptime(daily_trips$day, format = '%m-%d-%Y')
daily_trips$day <- as.POSIXct(daily_trips$day)


# wind speed data
windSpeed <- read.csv("wind_speed.csv")

windSpeed$datetime <- ymd_hms(windSpeed$datetime)
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
wind_vs_trips <- inner_join(hourly_trips, windSpeed_NYC, by = c('Hour' = 'Date'))
#join trips by day to temperature by day
temp_vs_trips <- inner_join(daily_trips, avg_daily_temp, by = c('day' = 'day'))


#linear regression statistics for temp vs trips
lin_reg_temp <- lm(trips_taken ~ temp_f, data = temp_vs_trips)
#manually looking at values from here to determine the regression eq
summary(lin_reg_temp)


#plotting data
ggplot(data = wind_vs_trips, aes(x = wind_vs_trips$`Wind Speed (mph)`, y = wind_vs_trips$trips_taken)) + geom_point(color = 'blue') + labs(x = 'Hourly Wind Speed', y = 'Trips taken in an hour', title = 'Jan. - Jun. 2015 Citibike Trips Compared to Wind Speed') + stat_smooth(method = "lm", col = 'black')


temp_corr <- ggplot(data = temp_vs_trips, aes(x = temp_vs_trips$temp_f, y = temp_vs_trips$trips_taken)) + geom_point(color = 'blue') + labs(x = 'Mean daily temperature, degrees Fahrenheit', y = 'Trips taken in a day', title = 'Jan. - Jun. 2015 Citibike Daily Ridership Compared to Temperature') + stat_smooth(method = "lm", col = 'black') + annotate("text", x = 60, y = 10000, label = "y = 473x - 2319, R2 = 0.75")

#save plot
ggsave(filename="temp_and_bike_ridership.png", plot=temp_corr)