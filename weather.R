# Weather Data Processing
library(dplyr)
library(lubridate)

# loading files
humidity <- read.csv("humidity.csv")
pressure <- read.csv("pressure.csv")
temperature <- read.csv("temperature.csv")
weatherDescription <- read.csv("weather_description.csv")
windSpeed <- read.csv("wind_speed.csv")

# humidity data for NYC
humidity$datetime <- ymd_hms(humidity$datetime)
humidity <- na.omit(humidity)
humidity <- subset(humidity,datetime>="2015-01-01" & datetime<="2016-06-01")
humidity_NYC <- select(humidity,datetime,New.York)
colnames(humidity_NYC) <- c("Date","Humidity (%)")

# pressure data for NYC
pressure$datetime <- ymd_hms(pressure$datetime)
pressure <- na.omit(pressure)
pressure <- subset(pressure,datetime>="2015-01-01" & datetime<="2016-06-01")
pressure_NYC <- select(pressure,datetime,New.York)
pressure_NYC$New.York <- pressure_NYC$New.York/1000
colnames(pressure_NYC) <- c("Date","Pressure (bar)")

# temperature data for NYC
temperature$datetime <- ymd_hms(temperature$datetime)
temperature <- na.omit(temperature)
temperature <- subset(temperature,datetime>="2015-01-01" & datetime<="2016-06-01")
temperature_NYC <- select(temperature,datetime,New.York)
temperature_NYC$New.York <- 9/5*(temperature_NYC$New.York-273)+32
colnames(temperature_NYC) <- c("Date","Temperature (F)")

# weather descriptions for NYC
weatherDescription$datetime <- ymd_hms(weatherDescription$datetime)
weatherDescription <- na.omit(weatherDescription)
weatherDescription <- subset(weatherDescription,datetime>="2015-01-01" & datetime<="2016-06-01")
weatherDescription_NYC <- select(weatherDescription,datetime,New.York)
colnames(weatherDescription_NYC) <- c("Date","Weather Description")

# wind speed data for NYC
windSpeed$datetime <- ymd_hms(windSpeed$datetime)
windSpeed <- na.omit(windSpeed)
windSpeed <- subset(windSpeed,datetime>="2015-01-01" & datetime<="2016-06-01")
windSpeed_NYC <- select(windSpeed,datetime,New.York)
windSpeed_NYC$New.York <- windSpeed_NYC$New.York*2.237
colnames(windSpeed_NYC) <- c("Date","Wind Speed (mph)")

# combining data
data_NYC <- inner_join(temperature_NYC,pressure_NYC,by="Date")
data_NYC <- inner_join(data_NYC,humidity_NYC,by="Date")
data_NYC <- inner_join(data_NYC,windSpeed_NYC,by="Date")
data_NYC <- inner_join(data_NYC,weatherDescription_NYC,by="Date")
