# Rideshare Data Analysis - WEATHER CONDITIONS
library(dplyr)
library(ggplot2)
library(lubridate)

setwd("~/COLLEGE/Spring 2019/HONR238V/Final Project")

uber <- read.csv("uber-raw-data-janjune-15.csv")
uber$Pickup_date <- ymd_hms(uber$Pickup_date)
colnames(uber) <- c("dispatching_base_num","datetime","affiliated_base_num","locationID")
uber <- na.omit(uber)
uber <- subset(uber,datetime>="2015-01-02" & datetime<="2015-06-01")
uber <- select(uber,datetime)
uber <- arrange(uber,datetime)

uber$hour <- round_date(uber$datetime, 'hour')
hourly_rides <- uber %>% group_by(uber$hour) %>% summarize(n())
colnames(hourly_rides) <- c('hour', 'rides')


library(rockchalk)

weatherDescription <- read.csv("weather_description.csv")
weatherDescription$datetime <- ymd_hms(weatherDescription$datetime)
weatherDescription <- na.omit(weatherDescription)
weatherDescription <- subset(weatherDescription,datetime>="2015-01-02" & datetime<="2015-06-01")
weatherDescription_NYC <- select(weatherDescription,datetime,New.York)
colnames(weatherDescription_NYC) <- c("datetime","description")
weatherDescription_NYC <- arrange(weatherDescription_NYC,datetime)

levels(weatherDescription_NYC$description) <- c(levels(weatherDescription_NYC$description), 'Clear or Cloudy', 'Light Rain', 'Moderate to Heavy Rain', 'Snow', 'Fog/Haze/Smoke')

weatherDescription_NYC$description <-combineLevels(weatherDescription_NYC$description, levs = c('sky is clear', 'few clouds', 'broken clouds', 'overcast clouds', 'scattered clouds'), newLabel = c('Clear or Cloudy'))
weatherDescription_NYC$description <- combineLevels(weatherDescription_NYC$description, levs = c('light rain'), newLabel = c('Light Rain'))
weatherDescription_NYC$description <- combineLevels(weatherDescription_NYC$description, levs = c('moderate rain', 'heavy intensity rain', 'very heavy rain'), newLabel = c('Moderate to Heavy Rain'))
weatherDescription_NYC$description <- combineLevels(weatherDescription_NYC$description, levs = c('light snow', 'snow'), newLabel = c('Snow'))
weatherDescription_NYC$description <- combineLevels(weatherDescription_NYC$description, levs = c('mist', 'fog', 'haze'), newLabel= c('Fog/Haze/Smoke'))


weather_vs_rides <- inner_join(hourly_rides, weatherDescription_NYC, by = c('hour' = 'datetime'))
weather_rides <- weather_vs_rides %>% group_by(weather_vs_rides$description) %>% summarise(trips = mean(rides), stdev = sd(rides))
colnames(weather_rides) <- c('desc', 'rides', 'stdev')

ggplot(data = weather_rides, aes(x = desc, y = rides)) + geom_bar(stat = 'identity') + labs(x = 'Weather Conditions in a Given Hour', y = 'Average Number of Hourly Trips Taken', title = 'Uber Hourly Ridership in Different Weather Conditions')
# ggplot(data = weather_rides, aes(x = desc, y = rides)) + geom_boxplot(width = 0.5, fill = 'steelblue', outlier.color = 'navyblue', notch = TRUE, position = 'dodge') + labs(x = 'Weather conditions in a given hour', y = 'average number of hourly trips taken', title = 'Uber Hourly Ridership in Different Weather Conditions')
