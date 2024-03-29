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

detach("package:rockchalk", unload=TRUE)

hourly_rides$time <- hour(hourly_rides$hour)

weather_vs_rides <- inner_join(hourly_rides, weatherDescription_NYC, by = c('hour' = 'datetime'))

weather_rides <- weather_vs_rides %>% group_by(description) %>% summarise(trips = mean(rides), stdev = sd(rides),n=n())
colnames(weather_rides) <- c('desc', 'rides', 'stdev','n')
weather_rides$error <- qnorm(0.975)*weather_rides$stdev/sqrt(weather_rides$n)

ggplot(data = weather_rides, aes(x = desc, y = rides)) + 
  geom_bar(stat = 'identity',width = 0.5,fill = 'steelblue') + 
  labs(x = 'Weather Conditions in a Given Hour', 
       y = 'Mean Number of Hourly Trips Taken', 
       title = 'Jan.-June 2015 Uber Hourly Ridership in Different Weather Conditions') + 
  geom_errorbar(data = weather_rides, aes(x = desc, 
                                          ymax = rides + error, 
                                          ymin = rides - error),
                width=0.2)
