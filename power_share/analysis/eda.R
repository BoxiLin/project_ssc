library(readxl)
library(tidyr)
library(ggplot2)
library(dplyr) 
library(lubridate)
library(forecast)

# Hourly: all sectors aggregated 
#         (residential + industrial + commercial/institutional + agriculture + transportation)
h_usage <-read_excel("data/SSC2020_hourly_demand.xlsx",2) 
names(h_usage)<-c("date", "hour","y_aggregate", "year","month")

# Annualy:  (residential: s.heat, w.heat, appliance, light, s.cooling
#            industrial, commercial/institutional, agriculture, transportation)
y_usage <-read.csv("data/ssc2020_annual_demand.csv")

# Hourly:
h_weather <- read_excel("data/ssc2020_hourly_weather.xlsx",2)

theta = y_usage$Residential / rowSums(y_usage[,3:11])
names(theta) <- 2003:2016

h_usage$theta <- theta[as.character(h_usage$year)]
h_usage$y <- h_usage$y_aggregate*h_usage$theta

usage <- filter(h_usage, year==2015)
y <- ts(usage$y,frequency=24)
weather <- filter(h_weather, year(time)==2015)

usage$y %>% mstl() %>%
  autoplot() + xlab("Hour")





mreg <- model.matrix(~as.factor(usage$month))[,2:12]
colnames(mreg) <- c('Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
hreg <- model.matrix(~as.factor(usage$hour))[,2:24]
colnames(hreg) <- paste("h",as.character(2:24), sep = "")
wreg <-  model.matrix(~as.factor(week(usage$date)))[,2:7]
colnames(wreg) <- c("Tue", "Wed", "Thur","Fri","Sat","Sun")
dreg <-  model.matrix(~as.factor(day(usage$date)))[,2:31]


data <- data.frame(y, mreg, wreg, weather[,2:9])

model1 <- Arima(data$y, order=c(2,0,0), 
               seasonal=list(order=c(1,0,0), period=24))
#              xreg=as.matrix(data[,2:26]))
accuracy(model1)

plot(data$y,type = "l")
lines(a, col=2)
lines(fitted(model1),col=2)
lines(fitted(model1),col=2)
lines(data$y)

model3 <- tslm(y~., data = data)
summary(model3)

autoplot(y) +
  autolayer(fitted(model), series="Data")





y %>% mstl() %>%
  autoplot()
