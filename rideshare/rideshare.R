# Ride Share Data Processing
library(dplyr)
library(lubridate)

# set working directory
setwd("~/COLLEGE/Spring 2019/HONR238V/Final Project")

# loading files
uber <- read.csv("uber-raw-data-janjune-15.csv")

# uber data
uber$Pickup_date <- ymd_hms(uber$Pickup_date)
colnames(uber) <- c("dispatching_base_num","Date","affiliated_base_num","location_ID")
uber <- na.omit(uber)
uber <- subset(uber,Date>="2015-01-01" & Date<="2015-06-01")
uber <- select(uber,Date)
