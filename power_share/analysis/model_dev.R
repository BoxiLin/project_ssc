library(ggplot2)
library(dplyr) 

# Hourly: all sectors aggregated 
#         (residential + industrial + commercial/institutional + agriculture + transportation)
h_usage <-readxl::read_excel("data/SSC2020_hourly_demand.xlsx",2) 
names(h_usage)<-c("date", "hour","y_aggregate", "year","month")

# Annualy:  (residential: s.heat, w.heat, appliance, light, s.cooling
#            industrial, commercial/institutional, agriculture, transportation)
y_usage <-read.csv("data/ssc2020_annual_demand.csv")

# Hourly weather:
h_weather <- readxl::read_excel("data/ssc2020_hourly_weather.xlsx",2)
theta = y_usage$Residential / rowSums(y_usage[,3:11])
names(theta) <- 2003:2016
h_usage$theta <- theta[as.character(h_usage$year)]
h_usage$y <- h_usage$y_aggregate*h_usage$theta

dummy_h <- model.matrix(~as.factor(h_usage$hour))[,2:24]
colnames(dummy_h) <- paste("h", 2:24 ,sep = "")
dummy_w <-  model.matrix(~as.factor(lubridate::week(h_usage$date)))[,2:7]
colnames(dummy_w) <- c("Tue", "Wed", "Thur","Fri","Sat","Sun")
dummy_m <- model.matrix(~as.factor(h_usage$month))[,2:12]
colnames(dummy_m) <- c('Feb','Mar','Apr','May','Jun','Jul',
                       'Aug','Sep','Oct','Nov','Dec')
data <- data.frame(dplyr::select(h_weather, -time), dummy_h, dummy_w, dummy_m, year = h_usage$year, y = h_usage$y)
str(data)


rm(list=setdiff(ls(), "data"))



# 1.1 XGBoost
mae = c()
mae_year = c()
importance = c()
for(i in 2003:2016){
  
  year_validate = i
  year_test <- if (year_validate<=2004) (year_validate+1):(year_validate+2) else if(year_validate==2016) 2014:2015 else{year_validate+c(-1,1)}
  year_train <- (2004:2016)[! (2004:2016) %in% c(year_validate, year_test)]
  message(Sys.time(),paste(c(">>> Validate:", year_validate, "; Train:", year_train, "; Test: ", year_test),collapse=" ",sep = ""))
  
  validate <- dplyr::filter(data, year==year_validate) 
  train <- dplyr::filter(data, year %in% year_train) 
  test <- dplyr::filter(data, year %in% year_test)
  
  dtrain = xgboost::xgb.DMatrix(data =as.matrix(select(train,-y)) , label = train$y)
  dtest = xgboost::xgb.DMatrix(data =as.matrix(select(test,-y)) , label = test$y)
  dvalidate = xgboost::xgb.DMatrix(data =as.matrix(select(validate,-y)) , label = validate$y)
  
  wl <- list(train = dtrain, test = dtest)
  model1  <- xgboost::xgb.train(data = dtrain, max.depth = 3, watchlist = wl,
                                early_stopping_rounds=50, eta = 0.5,  nrounds = 800,
                                objective = "reg:squarederror", verbose = 0)
  plot(model1$evaluation_log$iter, model1$evaluation_log$train_rmse, type = "l",
       main = paste("Evaluation log - predicting for year", year_validate),
       xlab = "Iter.", ylab = "RMSE")
  lines(model1$evaluation_log$iter, model1$evaluation_log$test_rmse, col = "red")
  legend("topright", legend = c("Train","Test"), fill = c("black", "red"))
  
  predicted = predict(model1, dvalidate)
  
  mae = c(mae,mean(abs(predicted-validate$y)))
  mae_year = c(mae_year,abs(sum(validate$y-predicted)))
  importance[[i-2002]] <- xgboost::xgb.importance(model = model1)
  
  forecast::autoplot(ts(validate$y,frequency = 24), xlab = "hour", ylab = "usage")+ 
    ggtitle(paste("Testing set fitting, predicting for year", year_validate)) + 
    autolayer(ts(predicted, frequency = 24), series = "XGBoost", col=2)
  ggsave(device = "png",width = 12, height = 2,units = "in",
         filename = paste(year_validate,"ts.png",sep = ""))
}






# # 1.2 Baseline model: ARIMA
# model1 <- Arima(training_y, order=c(2,0,0), 
#                 seasonal = list(order=c(1,0,0), period=24),
#                 xreg = xreg_train)
# accuracy(model1)
# 
# mean(abs(forecast(model1, 8760,xreg = xreg_test)$fitted-testing$y))
# 
# p_train <- autoplot(training_y, xlab = "hour", ylab = "usage")+ggtitle("Training set")+
#   autolayer(ts(forecast(model1,xreg = xreg_test,8760)$fitted, frequency = 24), 
#             series = "Baseline ARIMA", col=2)
# p_test <- autoplot(testing_y, xlab = "hour", ylab = "usage")+ ggtitle("Testing set") +
#   autolayer(ts(forecast(model1,xreg = xreg_test,8760)$fitted, frequency = 24), 
#             series = "Baseline ARIMA", col=2)
# gridExtra::grid.arrange(p_train,p_test, ncol=1)
# 
# 
# 
# # nonlinearity observed; add piece-wise linear term;
# # minimum: lowess(training_weather$temperature, training$y) 
# h_weather$temperature_summer <- pmax(h_weather$temperature,11.371)
# h_weather$air_density_summer <- pmax(h_weather$air_density,1.2387)
