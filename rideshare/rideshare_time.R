# Rideshare Data Analysis - TIME
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

uber$day <- round_date(uber$datetime, 'day')
daily_rides <- uber %>% group_by(uber$day) %>% summarize(n())
colnames(daily_rides) <- c('hour', 'rides')

uber$hour <- round_date(uber$datetime, 'hour')
hourly_rides <- uber %>% group_by(uber$hour) %>% summarize(n())
colnames(hourly_rides) <- c('hour', 'rides')

uber$time <- hour(uber$hour)

daily_pattern <- uber %>% group_by(uber$time) %>% summarize(n()/(nrow(daily_rides)))
colnames(daily_pattern) <- c('hour', 'mean_ridership')

ggplot(data = daily_pattern, aes(x = hour, y = mean_ridership)) + 
  geom_area(linetype = 'solid', fill = 'springgreen3') + 
  geom_point(col = 'springgreen4') + 
  labs(x = 'Hour of the Day', 
       y = 'Mean Ridership', 
       title = 'Jan.-June 2015 Uber Daily Ridership Pattern by Hour')
