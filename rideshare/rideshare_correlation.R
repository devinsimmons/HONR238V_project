# Rideshare Data Analysis
library(dplyr)
library(ggplot2)
library(lubridate)

setwd("~/COLLEGE/Spring 2019/HONR238V/Final Project")

temperature <- read.csv("temperature.csv")
temperature$datetime <- ymd_hms(temperature$datetime)
temperature <- na.omit(temperature)
temperature <- subset(temperature,datetime>="2015-01-02" & datetime<="2015-06-01")
temperature_NYC <- select(temperature,datetime,New.York)
temperature_NYC$New.York <- 9/5*(temperature_NYC$New.York-273)+32
colnames(temperature_NYC) <- c("datetime","temp_F")
temperature_NYC <- arrange(temperature_NYC,datetime)
temperature_NYC$date <- round_date(temperature_NYC$datetime, 'day')

avg_daily_temp <- temperature_NYC %>% group_by(temperature_NYC$date) %>% summarize(mean_daily_temp = mean(temp_F))
colnames(avg_daily_temp) <- c('day', 'temp_F')

uber <- read.csv("uber-raw-data-janjune-15.csv")
uber$Pickup_date <- ymd_hms(uber$Pickup_date)
colnames(uber) <- c("dispatching_base_num","datetime","affiliated_base_num","locationID")
uber <- na.omit(uber)
uber <- subset(uber,datetime>="2015-01-02" & datetime<="2015-06-01")
uber <- select(uber,datetime)
uber <- arrange(uber,datetime)
uber$date <- round_date(uber$datetime, 'day')

daily_rides <- uber %>% group_by(uber$date) %>% summarize(total_daily_rides = n())
colnames(daily_rides) <- c('day', 'rides')

temp_vs_rides <- inner_join(daily_rides, avg_daily_temp, by = c('day' = 'day'))
lin_reg_temp <- lm(rides ~ temp_F, data = temp_vs_rides)
summary(lin_reg_temp)

ggplot(data = temp_vs_rides, aes(x = temp_vs_rides$temp_F, y = temp_vs_rides$rides)) + geom_point(color = 'blue') + labs(x = 'Average Daily Temperature (degrees Fahrenheit)', y = 'Rides Taken in a Day', title = 'Uber Daily Ridership vs. Average Daily Temperature') + stat_smooth(method = "lm", col = 'black') + annotate("text", x = 55, y = 25000, label = "y = 158.28X + 69240.79, R2 = 0.0255")
