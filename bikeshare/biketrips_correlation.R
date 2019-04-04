library(dplyr)
library(ggplot2)
library(lubridate)
install.packages('rockchalk')
library(forcats)

setwd("C:/Users/Devin Simmons/Desktop/classes/HONR238V/data/")

june_trips <- read.csv("201506-citibike-tripdata.csv")
jan_trips <- read.csv("201501-citibike-tripdata.csv")
feb_trips <- read.csv("201502-citibike-tripdata.csv")
mar_trips <- read.csv("201503-citibike-tripdata.csv")
apr_trips <- read.csv("201504-citibike-tripdata.csv")
may_trips <- read.csv("201505-citibike-tripdata.csv")

trips_2015 <- rbind(jan_trips, feb_trips, mar_trips, apr_trips, may_trips, june_trips)

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

#function that can be used to read the weather files, limit it to just NYC in our 
#study range
#filepath is a string representing the filepath of the csv file you want to load
#var_name is a string that represents what you want to name the second column, ie
#the weather variable
weather_reader <- function(filepath, var_name) {
  df = read.csv(filepath)
  df <- na.omit(df)
  #lubridate gave me trouble for the temp file without this line
  #if (filepath == 'temperature.csv') {
  #df$datetime <- strptime(df$datetime, '%m/%d/%Y %H:%M:%S')
  #}
  df$datetime <- ymd_hms(df$datetime)
  df <- subset(df,datetime>="2015-01-01" & datetime<="2016-06-01")
  df_nyc <- select(df, datetime, New.York)
  colnames(df_nyc) <- c("Date", var_name)
  return(df_nyc)
}

#read windspeed
windSpeed <- weather_reader("wind_speed.csv", 'wind_speed_mph')
#convert speed to mph
windSpeed$wind_speed_mph <- windSpeed$wind_speed_mph*2.237


# temperature data for NYC
temperature <- weather_reader('temperature.csv', 'deg_fahrenheit')
temperature$deg_fahrenheit <- 9/5*(temperature$deg_fahrenheit-273)+32



#weather description data
weather_desc <- weather_reader('weather_description.csv', 'description')
#add new levels that will form the grouped levels
levels(weather_desc$description) <- c(levels(weather_desc$description), 'Clear or cloudy', 'Light Rain', 'Moderate to Heavy Rain', 'Snow', 'Fog/Haze/Smoke')


library(rockchalk)
#use rockchalk to group levels
#no rain, either clear or cloudy
weather_desc$description <-combineLevels(weather_desc$description, levs = c('sky is clear', 'few clouds', 'broken clouds', 'overcast clouds', 'scattered clouds'), newLabel = c('Clear or cloudy'))
#light rain
weather_desc$description <- combineLevels(weather_desc$description, levs = c('light intensity drizzle', 'drizzle', 'light rain'), newLabel = c('Light Rain'))
#moderate and heavy rain, storms
weather_desc$description <- combineLevels(weather_desc$description, levs = c('moderate rain', 'heavy intensity rain', 'very heavy rain', 'proximity thunderstorm','thunderstorm with light rain','thunderstorm','thunderstorm with heavy rain','thunderstorm with rain', 'heavy intensity drizzle'), newLabel = c('Moderate to Heavy Rain'))
#snow
weather_desc$description <- combineLevels(weather_desc$description, levs = c('light snow', 'snow', 'heavy snow'), newLabel = c('Snow'))
#fog, smoke, etc
weather_desc$description <- combineLevels(weather_desc$description, levs = c('mist', 'fog', 'smoke', 'haze'), newLabel= c('Fog/Haze/Smoke'))


#determines the average temperature for each day
avg_daily_temp <- temperature %>% group_by(temperature$Date) %>% summarize(mean_daily_temp = mean(deg_fahrenheit))
colnames(avg_daily_temp) <- c('day', 'temp_f')

#join trips by hour to windspeed by hour based on time of observation
wind_vs_trips <- inner_join(hourly_trips, windSpeed, by = c('Hour' = 'Date'))
#join trips by day to temperature by day
temp_vs_trips <- inner_join(daily_trips, avg_daily_temp, by = c('day' = 'day'))
#join hourly trips to weather description
desc_vs_trips <- inner_join(hourly_trips, weather_desc, by = c('Hour' = 'Date'))


#average the number of hourly trips given a certain weather condition
weather_trips <- desc_vs_trips %>% group_by(desc_vs_trips$description) %>% summarise(trips = mean(trips_taken), stdev = sd(trips_taken))
colnames(weather_trips) <- c('desc', 'trips', 'stdev')

#linear regression statistics for temp vs trips
lin_reg_temp <- lm(trips_taken ~ temp_f, data = temp_vs_trips)
#manually looking at values from here to determine the regression eq
summary(lin_reg_temp)


#plotting data
wind_corr <- ggplot(data = wind_vs_trips, aes(x = wind_vs_trips$wind_speed_mph, y = wind_vs_trips$trips_taken)) + geom_point(color = 'blue') + labs(x = 'Hourly Wind Speed', y = 'Trips taken in an hour', title = 'Jan. - Jun. 2015 Citibike Trips Compared to Wind Speed') + stat_smooth(method = "lm", col = 'black')


temp_corr <- ggplot(data = temp_vs_trips, aes(x = temp_vs_trips$temp_f, y = temp_vs_trips$trips_taken)) + geom_point(color = 'blue') + labs(x = 'Mean daily temperature, degrees Fahrenheit', y = 'Trips taken in a day', title = 'Jan. - Jun. 2015 Citibike Daily Ridership Compared to Temperature') + stat_smooth(method = "lm", col = 'black') + annotate("text", x = 60, y = 10000, label = "y = 473x - 2319, R2 = 0.75")

#plot bar graph
weather_plot <- ggplot(data = weather_trips, aes(x = desc, y = trips))+ geom_bar(stat = 'identity') + labs(x = 'Weather conditions in a given hour', y = 'Average number of hourly trips taken', title = 'Jan. - Jun. 2015 Citibike Average Hourly Ridership in Different Weather Conditions')
weather_plot


#save plot
ggsave(filename="temp_and_bike_ridership.png", plot=temp_corr)