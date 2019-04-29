# Rideshare Data Analysis - WIND SPEED
library(dplyr)
library(ggplot2)
library(lubridate)

setwd("~/COLLEGE/Spring 2019/HONR238V/Final Project")


windSpeed <- read.csv("wind_speed.csv")
windSpeed$datetime <- ymd_hms(windSpeed$datetime)
windSpeed <- na.omit(windSpeed)
windSpeed <- subset(windSpeed,datetime>="2015-01-02" & datetime<="2015-06-01")
windSpeed_NYC <- select(windSpeed,datetime,New.York)
windSpeed_NYC$New.York <- windSpeed_NYC$New.York*2.237
colnames(windSpeed_NYC) <- c("datetime","wind_mph")
windSpeed_NYC <- arrange(windSpeed_NYC,datetime)


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


wind_vs_rides <- inner_join(hourly_rides, windSpeed_NYC, by = c('hour' = 'datetime'))
wind_vs_rides$wind_mph <- as.factor(wind_vs_rides$wind_mph)

ggplot(data = wind_vs_rides, aes(x = wind_mph, y = rides)) + 
  geom_boxplot(width = 0.5, fill = 'steelblue', outlier.color = 'navyblue', 
                 notch = FALSE, position = 'dodge') + 
  labs(x = 'Hourly Wind Speed (MPH)', 
       y = 'Trips Taken in an Hour', 
       title = 'Jan.-June 2015 Uber Hourly Rides Compared to Wind Speed')
