library(readxl)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(forecast)
library(xts)
library(foreach)
library(doParallel)
#library(iterators)
#setwd('~/SSC 2020/elec/')

# Calculate the number of cores
no_cores <- min(detectCores()-2,40)
print(paste("No.core:", no_cores))

# Hourly: all sectors aggregated 
#         (residential + industrial + commercial/institutional + agriculture + transportation)
h_usage <- read_excel("SSC2020_hourly_demand.xlsx",2)

# Annualy:  (residential: s.heat, w.heat, appliance, light, s.cooling
#            industrial, commercial/institutional, agriculture, transportation)
y_usage <- read.csv("ssc2020_annual_demand.csv")

# Hourly:
h_weather <- read_excel("ssc2020_hourly_weather.xlsx",2)
names(h_usage) <- c("date", "hour","y", "year","month")
n_year = 14

### Reformat Date to be consistent for both h_usage and h_weather
h_usage <- h_usage %>%
  mutate(date=ymd(date)) %>%
  mutate(day=day(date)) %>%
  mutate(time=make_datetime(year,month,day,(hour-1)))
h_dat <- left_join(h_usage,h_weather,by='time')

# Annual demand across sectors and residential proportion
y_usage <- y_usage %>%
  mutate(Total=Residential+Industrial+Commercial+Agriculture+Transportation) %>%
  mutate(resprop=Residential/Total)
temp <- y_usage %>% 
  select(year=Year,resprop)
h_dat <- left_join(h_dat,temp,by='year') 
h_dat <- h_dat %>%
  mutate(res=y*resprop)

ts.res <- xts(x=h_dat$res, order.by=h_dat$time, frequency=24)
ts.precip <- xts(x=h_dat$precipitation, order.by=h_dat$time, frequency=24)
ts.temp <- xts(x=h_dat$temperature, order.by=h_dat$time, frequency=24)
ts.irrad.surf <- xts(x=h_dat$irradiance_surface, order.by=h_dat$time, frequency=24)
ts.irrad.atmo <- xts(x=h_dat$irradiance_toa, order.by=h_dat$time, frequency=24)
ts.snowf <- xts(x=h_dat$snowfall, order.by=h_dat$time, frequency=24)
ts.snowd <- xts(x=h_dat$snow_depth, order.by=h_dat$time, frequency=24)
ts.cloud <- xts(x=h_dat$cloud_cover, order.by=h_dat$time, frequency=24)
ts.airden <- xts(x=h_dat$air_density, order.by=h_dat$time, frequency=24)

### Dynamic Harmonic Regression ###

# This is the better harm.reg function, but not feasible given
# lack of computational resources

# harm.reg <- function(train,train.msts,a=20,b=20,c=20,cov=NULL){
#   i.iter <- 
#     if(a%%2==0){
#       iter(seq(2,a,by=2))
#     } else{
#       iter(seq(2,20,by=2))
#     }
#   j.iter <- 
#     if(b%%2==0){
#       iter(seq(2,b,by=2))
#     } else{
#       iter(seq(2,20,by=2))
#     }
#   k.iter <- 
#     if(c%%2==0){
#       iter(seq(2,c,by=2))
#     } else{
#       iter(seq(2,20,by=2))
#     }
#   res <- 
#     foreach (i=i.iter,.packages ='forecast',.multicombine = TRUE) %:%
#     foreach (j=j.iter,.packages ='forecast',.multicombine = TRUE) %:%
#     foreach (k=k.iter,.packages ='forecast',.multicombine = TRUE) %dopar%{
#       fit <- auto.arima(train, xreg=cbind(fourier(train.msts, K=c(i,j,k)), cov), seasonal=FALSE)
#     }
#   return(res)
# }

# Uses auto.arima to fit best dynamic harmonic regression model with given cov for different
# numbers of Fourier terms
harm.reg <- function(train,train.msts,a=4,b=4,c=4,cov=NULL){
  res <- 
    foreach (i=1:a,.packages ='forecast',.multicombine = TRUE) %:%
    foreach (j=1:b,.packages ='forecast',.multicombine = TRUE) %:%
    foreach (k=1:c,.packages ='forecast',.multicombine = TRUE) %dopar%{
      fit <- auto.arima(train, xreg=cbind(fourier(train.msts, K=c(i,j,k)), cov), seasonal=FALSE)
    }
  return(res)
}

a <- 4
b <- 4
c <- 4
no.yrs <- 13

best.precip <- vector("list", no.yrs)
ijk.precip <- vector("list", no.yrs)
best.temp <- vector("list", no.yrs)
ijk.temp <- vector("list", no.yrs)
best.irrad.surf <- vector("list", no.yrs)
ijk.irrad.surf <- vector("list", no.yrs)
best.irrad.atmo <- vector("list", no.yrs)
ijk.irrad.atmo <- vector("list", no.yrs)
best.snowf <- vector("list", no.yrs)
ijk.snowf <- vector("list", no.yrs)
best.snowd <- vector("list", no.yrs)
ijk.snowd <- vector("list", no.yrs)
best.cloud <- vector("list", no.yrs)
ijk.cloud <- vector("list", no.yrs)
best.airden <- vector("list", no.yrs)
ijk.airden <- vector("list", no.yrs)
best.cov1 <- vector("list", no.yrs)
ijk.cov1 <- vector("list", no.yrs)
best.cov2 <- vector("list", no.yrs)
ijk.cov2 <- vector("list", no.yrs)
best.cov3 <- vector("list", no.yrs)
ijk.cov3 <- vector("list", no.yrs)
best.cov4 <- vector("list", no.yrs)
ijk.cov4 <- vector("list", no.yrs)
fc.precip <- vector("list", no.yrs)
fc.temp <- vector("list", no.yrs)
fc.irrad.surf <- vector("list", no.yrs)
fc.irrad.atmo <- vector("list", no.yrs)
fc.snowf <- vector("list", no.yrs)
fc.snowd <- vector("list", no.yrs)
fc.cloud <- vector("list", no.yrs)
fc.airden <- vector("list", no.yrs)
fc.cov1 <- vector("list", no.yrs)
fc.cov2 <- vector("list", no.yrs)
fc.cov3 <- vector("list", no.yrs)
fc.cov4 <- vector("list", no.yrs)

# main for loop
for (yr in 4:(4+no.yrs-1)) { # predictions for year 2004 to 2016
  registerDoParallel(no_cores)
  train.res <- ts(ts.res[paste('2003-01-01/',2000+yr-1,'-12-31',sep='')],frequency=1)
  train.res.msts <- msts(train.res, seasonal.periods=c(24,168,8760))
  test.res <- ts(ts.res[paste(2000+yr,'-01-01/',2000+yr,'-12-31',sep='')],frequency=1)
  test.res.msts <- msts(test.res, c(24,168,8760))
  
  # precipitation as cov
  train.cov <- coredata(ts.precip[paste('2003-01-01/',2000+yr-1,'-12-31',sep='')])
  test.cov <- coredata(ts.precip[paste(2000+yr,'-01-01/',2000+yr,'-12-31',sep='')])
  print(Sys.time())
  system.time({
    result <- harm.reg(train.res,train.res.msts,a,b,c,train.cov)
  })
  bestfit <- list(aicc=Inf)
  for(i in 1:a){
    for(j in 1:b){
      for(k in 1:c){
        fitted <- result[[i]][[j]][[k]]
        if(fitted$aicc < bestfit$aicc) {
          bestfit <- fitted
          best.ijk <- c(i,j,k)
        }
      }
    }
  }
  best.precip[[yr-3]] <- bestfit
  ijk.precip[[yr-3]] <- best.ijk
  fc.precip[[yr-3]] <- forecast(best.precip[[yr-3]],
                                xreg=cbind(fourier(train.res.msts, K=ijk.precip[[yr-3]], h=nrow(test.cov)),test.cov))
#  # autoplot(fc.precip[[yr-3]])
#  # ggsave(paste('fc.precip',yr,'.png',sep=''))
  saveRDS(fc.precip, file = paste('fc.precip',yr,'.rds',sep=''))
  
  # temperature as cov
  train.cov <- coredata(ts.temp[paste('2003-01-01/',2000+yr-1,'-12-31',sep='')])
  test.cov <- coredata(ts.temp[paste(2000+yr,'-01-01/',2000+yr,'-12-31',sep='')])
  print(Sys.time())
  system.time({
    result <- harm.reg(train.res,train.res.msts,a,b,c,train.cov)
  })
  bestfit <- list(aicc=Inf)
  for(i in 1:a){
    for(j in 1:b){
      for(k in 1:c){
        fitted <- result[[i]][[j]][[k]]
        if(fitted$aicc < bestfit$aicc) {
          bestfit <- fitted
          best.ijk <- c(i,j,k)
        }
      }
    }
  }
  best.temp[[yr-3]] <- bestfit
  ijk.temp[[yr-3]] <- best.ijk
  fc.temp[[yr-3]] <- forecast(best.temp[[yr-3]],
                              xreg=cbind(fourier(train.res.msts, K=ijk.temp[[yr-3]], h=nrow(test.cov)),test.cov))
  # autoplot(fc.temp[[yr-3]])
  # ggsave(paste('fc.temp',yr,'.png',sep=''))
  saveRDS(fc.temp, file = paste('fc.temp',yr,'.rds',sep=''))
  
  # irradiance at surface as cov
  train.cov <- coredata(ts.irrad.surf[paste('2003-01-01/',2000+yr-1,'-12-31',sep='')])
  test.cov <- coredata(ts.irrad.surf[paste(2000+yr,'-01-01/',2000+yr,'-12-31',sep='')])
  print(Sys.time())
  system.time({
    result <- harm.reg(train.res,train.res.msts,a,b,c,train.cov)
  })
  bestfit <- list(aicc=Inf)
  for(i in 1:a){
    for(j in 1:b){
      for(k in 1:c){
        fitted <- result[[i]][[j]][[k]]
        if(fitted$aicc < bestfit$aicc) {
          bestfit <- fitted
          best.ijk <- c(i,j,k)
        }
      }
    }
  }
  best.irrad.surf[[yr-3]] <- bestfit
  ijk.irrad.surf[[yr-3]] <- best.ijk
  fc.irrad.surf[[yr-3]] <- forecast(best.irrad.surf[[yr-3]],
                                    xreg=cbind(fourier(train.res.msts, K=ijk.irrad.surf[[yr-3]], h=nrow(test.cov)),test.cov))
  # autoplot(fc.irrad.surf[[yr-3]])
  # ggsave(paste('fc.irrad.surf',yr,'.png',sep=''))
  saveRDS(fc.irrad.surf, file = paste('fc.irrad.surf',yr,'.rds',sep=''))
  
  # irradiance top of atomosphere as cov
  train.cov <- coredata(ts.irrad.atmo[paste('2003-01-01/',2000+yr-1,'-12-31',sep='')])
  test.cov <- coredata(ts.irrad.atmo[paste(2000+yr,'-01-01/',2000+yr,'-12-31',sep='')])
  print(Sys.time())
  system.time({
    result <- harm.reg(train.res,train.res.msts,a,b,c,train.cov)
  })
  bestfit <- list(aicc=Inf)
  for(i in 1:a){
    for(j in 1:b){
      for(k in 1:c){
        fitted <- result[[i]][[j]][[k]]
        if(fitted$aicc < bestfit$aicc) {
          bestfit <- fitted
          best.ijk <- c(i,j,k)
        }
      }
    }
  }
  best.irrad.atmo[[yr-3]] <- bestfit
  ijk.irrad.atmo[[yr-3]] <- best.ijk
  fc.irrad.atmo[[yr-3]] <- forecast(best.irrad.atmo[[yr-3]],
                                    xreg=cbind(fourier(train.res.msts, K=ijk.irrad.atmo[[yr-3]], h=nrow(test.cov)),test.cov))
  # autoplot(fc.irrad.atmo[[yr-3]])
  # ggsave(paste('fc.irrad.atmo',yr,'.png',sep=''))
  saveRDS(fc.irrad.atmo, file = paste('fc.irrad.atmo',yr,'.rds',sep=''))
  
  # snowfall as cov
  train.cov <- coredata(ts.snowf[paste('2003-01-01/',2000+yr-1,'-12-31',sep='')])
  test.cov <- coredata(ts.snowf[paste(2000+yr,'-01-01/',2000+yr,'-12-31',sep='')])
  print(Sys.time())
  system.time({
    result <- harm.reg(train.res,train.res.msts,a,b,c,train.cov)
  })
  bestfit <- list(aicc=Inf)
  for(i in 1:a){
    for(j in 1:b){
      for(k in 1:c){
        fitted <- result[[i]][[j]][[k]]
        if(fitted$aicc < bestfit$aicc) {
          bestfit <- fitted
          best.ijk <- c(i,j,k)
        }
      }
    }
  }
  best.snowf[[yr-3]] <- bestfit
  ijk.snowf[[yr-3]] <- best.ijk
  fc.snowf[[yr-3]] <- forecast(best.snowf[[yr-3]],
                               xreg=cbind(fourier(train.res.msts, K=ijk.snowf[[yr-3]], h=nrow(test.cov)),test.cov))
  # autoplot(fc.snowf[[yr-3]])
  # ggsave(paste('fc.snowf',yr,'.png',sep=''))
  saveRDS(fc.snowf, file = paste('fc.snowf',yr,'.rds',sep=''))
  
  # snow depth as cov
  train.cov <- coredata(ts.snowd[paste('2003-01-01/',2000+yr-1,'-12-31',sep='')])
  test.cov <- coredata(ts.snowd[paste(2000+yr,'-01-01/',2000+yr,'-12-31',sep='')])
  print(Sys.time())
  system.time({
    result <- harm.reg(train.res,train.res.msts,a,b,c,train.cov)
  })
  bestfit <- list(aicc=Inf)
  for(i in 1:a){
    for(j in 1:b){
      for(k in 1:c){
        fitted <- result[[i]][[j]][[k]]
        if(fitted$aicc < bestfit$aicc) {
          bestfit <- fitted
          best.ijk <- c(i,j,k)
        }
      }
    }
  }
  best.snowd[[yr-3]] <- bestfit
  ijk.snowd[[yr-3]] <- best.ijk
  fc.snowd[[yr-3]] <- forecast(best.snowd[[yr-3]],
                               xreg=cbind(fourier(train.res.msts, K=ijk.snowd[[yr-3]], h=nrow(test.cov)),test.cov))
  # autoplot(fc.snowd[[yr-3]])
  # ggsave(paste('fc.snowd',yr,'.png',sep=''))
  saveRDS(fc.snowd, file = paste('fc.snowd',yr,'.rds',sep=''))
  
  # cloud cover as cov
  train.cov <- coredata(ts.cloud[paste('2003-01-01/',2000+yr-1,'-12-31',sep='')])
  test.cov <- coredata(ts.cloud[paste(2000+yr,'-01-01/',2000+yr,'-12-31',sep='')])
  print(Sys.time())
  system.time({
    result <- harm.reg(train.res,train.res.msts,a,b,c,train.cov)
  })
  bestfit <- list(aicc=Inf)
  for(i in 1:a){
    for(j in 1:b){
      for(k in 1:c){
        fitted <- result[[i]][[j]][[k]]
        if(fitted$aicc < bestfit$aicc) {
          bestfit <- fitted
          best.ijk <- c(i,j,k)
        }
      }
    }
  }
  best.cloud[[yr-3]] <- bestfit
  ijk.cloud[[yr-3]] <- best.ijk
  fc.cloud[[yr-3]] <- forecast(best.cloud[[yr-3]],
                               xreg=cbind(fourier(train.res.msts, K=ijk.cloud[[yr-3]], h=nrow(test.cov)),test.cov))
  # autoplot(fc.cloud[[yr-3]])
  # ggsave(paste('fc.cloud',yr,'.png',sep=''))
  saveRDS(fc.cloud, file = paste('fc.cloud',yr,'.rds',sep=''))
  
  # air density as cov
  train.cov <- coredata(ts.airden[paste('2003-01-01/',2000+yr-1,'-12-31',sep='')])
  test.cov <- coredata(ts.airden[paste(2000+yr,'-01-01/',2000+yr,'-12-31',sep='')])
  print(Sys.time())
  system.time({
    result <- harm.reg(train.res,train.res.msts,a,b,c,train.cov)
  })
  bestfit <- list(aicc=Inf)
  for(i in 1:a){
    for(j in 1:b){
      for(k in 1:c){
        fitted <- result[[i]][[j]][[k]]
        if(fitted$aicc < bestfit$aicc) {
          bestfit <- fitted
          best.ijk <- c(i,j,k)
        }
      }
    }
  }
  best.airden[[yr-3]] <- bestfit
  ijk.airden[[yr-3]] <- best.ijk
  fc.airden[[yr-3]] <- forecast(best.airden[[yr-3]],
                                xreg=cbind(fourier(train.res.msts, K=ijk.airden[[yr-3]], h=nrow(test.cov)),test.cov))
  # autoplot(fc.airden[[yr-3]])
  # ggsave(paste('fc.airden',yr,'.png',sep=''))
  saveRDS(fc.airden, file = paste('fc.airden',yr,'.rds',sep=''))
  
  # temperature + irradiance at surface as cov
  train.cov <- cbind(coredata(ts.temp[paste('2003-01-01/',2000+yr-1,'-12-31',sep='')]),
                     coredata(ts.irrad.surf[paste('2003-01-01/',2000+yr-1,'-12-31',sep='')]))
  test.cov <- cbind(coredata(ts.temp[paste(2000+yr,'-01-01/',2000+yr,'-12-31',sep='')]),
                    coredata(ts.irrad.surf[paste(2000+yr,'-01-01/',2000+yr,'-12-31',sep='')]))
  print(Sys.time())
  system.time({
    result <- harm.reg(train.res,train.res.msts,a,b,c,train.cov)
  })
  bestfit <- list(aicc=Inf)
  for(i in 1:a){
    for(j in 1:b){
      for(k in 1:c){
        fitted <- result[[i]][[j]][[k]]
        if(fitted$aicc < bestfit$aicc) {
          bestfit <- fitted
          best.ijk <- c(i,j,k)
        }
      }
    }
  }
  best.cov1[[yr-3]] <- bestfit
  ijk.cov1[[yr-3]] <- best.ijk
  fc.cov1[[yr-3]] <- forecast(best.cov1[[yr-3]],
                              xreg=cbind(fourier(train.res.msts, K=ijk.cov1[[yr-3]], h=nrow(test.cov)),test.cov))
  # autoplot(fc.cov1[[yr-3]])
  # ggsave(paste('fc.cov1',yr,'.png',sep=''))
  saveRDS(fc.cov1, file = paste('fc.cov1',yr,'.rds',sep=''))
  
  # temperature + snow depth as cov
  train.cov <- cbind(coredata(ts.temp[paste('2003-01-01/',2000+yr-1,'-12-31',sep='')]),
                     coredata(ts.snowd[paste('2003-01-01/',2000+yr-1,'-12-31',sep='')]))
  test.cov <- cbind(coredata(ts.temp[paste(2000+yr,'-01-01/',2000+yr,'-12-31',sep='')]),
                    coredata(ts.snowd[paste(2000+yr,'-01-01/',2000+yr,'-12-31',sep='')]))
  print(Sys.time())
  system.time({
    result <- harm.reg(train.res,train.res.msts,a,b,c,train.cov)
  })
  bestfit <- list(aicc=Inf)
  for(i in 1:a){
    for(j in 1:b){
      for(k in 1:c){
        fitted <- result[[i]][[j]][[k]]
        if(fitted$aicc < bestfit$aicc) {
          bestfit <- fitted
          best.ijk <- c(i,j,k)
        }
      }
    }
  }
  best.cov2[[yr-3]] <- bestfit
  ijk.cov2[[yr-3]] <- best.ijk
  fc.cov2[[yr-3]] <- forecast(best.cov2[[yr-3]],
                              xreg=cbind(fourier(train.res.msts, K=ijk.cov2[[yr-3]], h=nrow(test.cov)),test.cov))
  # autoplot(fc.cov2[[yr-3]])
  # ggsave(paste('fc.cov2',yr,'.png',sep=''))
  saveRDS(fc.cov2, file = paste('fc.cov2',yr,'.rds',sep=''))
  
  # snow depth + irradiance at surface as cov
  train.cov <- cbind(coredata(ts.snowd[paste('2003-01-01/',2000+yr-1,'-12-31',sep='')]),
                     coredata(ts.irrad.surf[paste('2003-01-01/',2000+yr-1,'-12-31',sep='')]))
  test.cov <- cbind(coredata(ts.snowd[paste(2000+yr,'-01-01/',2000+yr,'-12-31',sep='')]),
                    coredata(ts.irrad.surf[paste(2000+yr,'-01-01/',2000+yr,'-12-31',sep='')]))
  print(Sys.time())
  system.time({
    result <- harm.reg(train.res,train.res.msts,a,b,c,train.cov)
  })
  bestfit <- list(aicc=Inf)
  for(i in 1:a){
    for(j in 1:b){
      for(k in 1:c){
        fitted <- result[[i]][[j]][[k]]
        if(fitted$aicc < bestfit$aicc) {
          bestfit <- fitted
          best.ijk <- c(i,j,k)
        }
      }
    }
  }
  best.cov3[[yr-3]] <- bestfit
  ijk.cov3[[yr-3]] <- best.ijk
  fc.cov3[[yr-3]] <- forecast(best.cov3[[yr-3]],
                              xreg=cbind(fourier(train.res.msts, K=ijk.cov3[[yr-3]], h=nrow(test.cov)),test.cov))
  # autoplot(fc.cov3[[yr-3]])
  # ggsave(paste('fc.cov3',yr,'.png',sep=''))
  saveRDS(fc.cov3, file = paste('fc.cov3',yr,'.rds',sep=''))
  
  # temperature + irradiance at surface + snow depth as cov
  train.cov <- cbind(coredata(ts.temp[paste('2003-01-01/',2000+yr-1,'-12-31',sep='')]),
                     coredata(ts.irrad.surf[paste('2003-01-01/',2000+yr-1,'-12-31',sep='')]),
                     coredata(ts.snowd[paste('2003-01-01/',2000+yr-1,'-12-31',sep='')]))
  test.cov <- cbind(coredata(ts.temp[paste(2000+yr,'-01-01/',2000+yr,'-12-31',sep='')]),
                    coredata(ts.irrad.surf[paste(2000+yr,'-01-01/',2000+yr,'-12-31',sep='')]),
                    coredata(ts.snowd[paste(2000+yr,'-01-01/',2000+yr,'-12-31',sep='')]))
  print(Sys.time())
  system.time({
    result <- harm.reg(train.res,train.res.msts,a,b,c,train.cov)
  })
  bestfit <- list(aicc=Inf)
  for(i in 1:a){
    for(j in 1:b){
      for(k in 1:c){
        fitted <- result[[i]][[j]][[k]]
        if(fitted$aicc < bestfit$aicc) {
          bestfit <- fitted
          best.ijk <- c(i,j,k)
        }
      }
    }
  }
  best.cov4[[yr-3]] <- bestfit
  ijk.cov4[[yr-3]] <- best.ijk
  fc.cov4[[yr-3]] <- forecast(best.cov4[[yr-3]],
                              xreg=cbind(fourier(train.res.msts, K=ijk.cov4[[yr-3]], h=nrow(test.cov)),test.cov))
  # autoplot(fc.cov4[[yr-3]])
  # ggsave(paste('fc.cov4',yr,'.png',sep=''))
  saveRDS(fc.cov4, file = paste('fc.cov4',yr,'.rds',sep=''))
  
  stopImplicitCluster()
}

# Find overall best model for each year by lowest AICc among seven candidate models 
# and compute MAE for forecasting residential sector usage in each year given previous years' data
yr.fc <- rep(0,no.yrs)
yr.usage <- rep(0,no.yrs)
best.cov <- rep(0,no.yrs)
best.model <- vector("list", no.yrs)
best.ijk <- vector("list", no.yrs)

for (yr in 1:no.yrs){
  AICc <- c(best.temp[[yr]]$aicc,best.irrad.surf[[yr]]$aicc,best.snowd[[yr]]$aicc,
            best.cov1[[yr]]$aicc,best.cov2[[yr]]$aicc,best.cov3[[yr]]$aicc,
            best.cov4[[yr]]$aicc)
  fc <- list(fc.temp[[yr]],fc.irrad.surf[[yr]],fc.snowd[[yr]],
             fc.cov1[[yr]],fc.cov2[[yr]],fc.cov3[[yr]],
             fc.cov4[[yr]])[[match(min(AICc),AICc)]]
  
  # record best covariate combo:
  # 1 = temp, 2 = irrad.surf, 3 = snowd, 4 = cov1, 5 = cov2, 6 = cov3, 7 = cov4
  best.cov[yr] <- match(min(AICc),AICc)
  
  # record summary of best fitted model by AICc
  best.model[[yr]] <- list(best.temp[[yr]],best.irrad.surf[[yr]],best.snowd[[yr]],
                           best.cov1[[yr]],best.cov2[[yr]],best.cov3[[yr]],
                           best.cov4[[yr]])[[match(min(AICc),AICc)]]
  
  # record best number of Fourier terms for each seasonality with max of four sets per seasonality
  # i = daily, j = weekly, k = annually
  best.ijk[[yr]] <- list(ijk.temp[[yr]],ijk.irrad.surf[[yr]],ijk.snowd[[yr]],
                         ijk.cov1[[yr]],ijk.cov2[[yr]],ijk.cov3[[yr]],
                         ijk.cov4[[yr]])[[match(min(AICc),AICc)]]
  
  yr.fc[yr] <- sum(fc$mean,na.rm=T)
  yr.usage[yr] <- sum(h_dat$res[h_dat$year==2003+yr])
}
# Finally, compute MAE in Megawatt hour units
MAE <- (1/no.yrs)*sum(abs(yr.usage-yr.fc))

# Save objects of interest to file
saveRDS(best.cov, file='best.cov.rds')
saveRDS(best.model, file='best.model.rds')
saveRDS(best.ijk, file='best.ijk.rds')
saveRDS(yr.fc, file='yr.fc.rds')
saveRDS(yr.usage, file='yr.usage.rds')
saveRDS(MAE, file='MAE.rds')

saveRDS(best.precip, file='best.precip.rds')
saveRDS(ijk.precip, file='ijk.precip.rds')
saveRDS(best.temp, file='best.temp.rds')
saveRDS(ijk.temp, file='ijk.temp.rds')
saveRDS(best.irrad.surf, file='best.irrad.surf.rds')  
saveRDS(ijk.irrad.surf, file='ijk.irrad.surf.rds')
saveRDS(best.irrad.atmo, file='best.irrad.atmo.rds')
saveRDS(ijk.irrad.atmo, file='ijk.irrad.atmo.rds')
saveRDS(best.snowf, file='best.snowf.rds')
saveRDS(ijk.snowf, file='ijk.snowf.rds')
saveRDS(best.snowd, file='best.snowd.rds')
saveRDS(ijk.snowd, file='ijk.snowd.rds')
saveRDS(best.cloud, file='best.cloud.rds')
saveRDS(ijk.cloud, file='ijk.cloud.rds') 
saveRDS(best.airden, file='best.airden.rds') 
saveRDS(ijk.airden, file='ijk.airden.rds') 
saveRDS(best.cov1, file='best.cov1.rds')
saveRDS(ijk.cov1, file='ijk.cov1.rds')
saveRDS(best.cov2, file='best.cov2.rds')
saveRDS(ijk.cov2, file='ijk.cov2.rds') 
saveRDS(best.cov3, file='best.cov3.rds')
saveRDS(ijk.cov3, file='ijk.cov3.rds')
saveRDS(best.cov4, file='best.cov4.rds')
saveRDS(ijk.cov4, file='ijk.cov4.rds')
saveRDS(fc.precip, file='fc.precip.rds')
saveRDS(fc.temp, file='fc.temp.rds')
saveRDS(fc.irrad.surf, file='fc.irrad.surf.rds')
saveRDS(fc.irrad.atmo, file='fc.irrad.atmo.rds')
saveRDS(fc.snowf, file='fc.snowf.rds') 
saveRDS(fc.snowd, file='fc.snowd.rds')
saveRDS(fc.cloud, file='fc.cloud.rds') 
saveRDS(fc.airden, file='fc.airden.rds') 
saveRDS(fc.cov1, file='fc.cov1.rds')
saveRDS(fc.cov2, file='fc.cov2.rds')
saveRDS(fc.cov3, file='fc.cov3.rds')
saveRDS(fc.cov4, file='fc.cov4.rds')
