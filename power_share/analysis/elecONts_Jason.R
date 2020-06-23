library(readxl)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(forecast)
library(xts)
library(foreach)
library(doParallel)
setwd('~/SSC 2020/elec/')

# Calculate the number of cores
no_cores <- detectCores()-1

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

harm.reg <- function(train,train.msts,a=3,b=3,c=3,cov=NULL){
  res <- 
    foreach(i=1:a,.packages ='forecast',.multicombine = TRUE) %:%
    foreach (j=1:b,.packages ='forecast',.multicombine = TRUE) %:%
    foreach (k=1:c,.packages ='forecast',.multicombine = TRUE) %dopar%{
      fit <- auto.arima(train, xreg=cbind(fourier(train.msts, K=c(i,j,k)), cov), seasonal=FALSE)
    }
  return(res)
}

a <- 3
b <- 3
c <- 3
best.precip <- vector("list", 13)
ijk.precip <- vector("list", 13)
best.temp <- vector("list", 13)
ijk.temp <- vector("list", 13)
best.irrad.surf <- vector("list", 13)
ijk.irrad.surf <- vector("list", 13)
best.irrad.atmo <- vector("list", 13)
ijk.irrad.atmo <- vector("list", 13)
best.snowf <- vector("list", 13)
ijk.snowf <- vector("list", 13)
best.snowd <- vector("list", 13)
ijk.snowd <- vector("list", 13)
best.cloud <- vector("list", 13)
ijk.cloud <- vector("list", 13)
best.airden <- vector("list", 13)
ijk.airden <- vector("list", 13)
best.cov1 <- vector("list", 13)
ijk.cov1 <- vector("list", 13)
best.cov2 <- vector("list", 13)
ijk.cov2 <- vector("list", 13)
best.cov3 <- vector("list", 13)
ijk.cov3 <- vector("list", 13)
best.cov4 <- vector("list", 13)
ijk.cov4 <- vector("list", 13)
fc.precip <- vector("list", 13)
fc.temp <- vector("list", 13)
fc.irrad.surf <- vector("list", 13)
fc.irrad.atmo <- vector("list", 13)
fc.snowf <- vector("list", 13)
fc.snowd <- vector("list", 13)
fc.cloud <- vector("list", 13)
fc.airden <- vector("list", 13)
fc.cov1 <- vector("list", 13)
fc.cov2 <- vector("list", 13)
fc.cov3 <- vector("list", 13)
fc.cov4 <- vector("list", 13)

# main for loop
for (yr in 5:16) { # predictions for year 2004 to 2016
  registerDoParallel(no_cores)
  train.res <- ts(ts.res[paste('2003-01-01/',2000+yr-1,'-12-31',sep='')],frequency=1)
  train.res.msts <- msts(train.res, c(24,168,8761))
  # test.res <- ts(ts.res[paste(2000+yr,'-01-01/',2000+yr,'-12-31',sep='')],frequency=1)
  # test.res.msts <- msts(test.res, c(24,168,8785))
  
  # precipitation as cov
  train.cov <- coredata(ts.precip[paste('2003-01-01/',2000+yr-1,'-12-31',sep='')])
  test.cov <- coredata(ts.precip[paste(2000+yr,'-01-01/',2000+yr,'-12-31',sep='')])
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
  autoplot(fc.precip[[yr-3]])
  ggsave(paste('fc.precip',yr,'.png',sep=''))
  
  # temperature as cov
  train.cov <- coredata(ts.temp[paste('2003-01-01/',2000+yr-1,'-12-31',sep='')])
  test.cov <- coredata(ts.temp[paste(2000+yr,'-01-01/',2000+yr,'-12-31',sep='')])
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
  autoplot(fc.temp[[yr-3]])
  ggsave(paste('fc.temp',yr,'.png',sep=''))
  
  # irradiance at surface as cov
  train.cov <- coredata(ts.irrad.surf[paste('2003-01-01/',2000+yr-1,'-12-31',sep='')])
  test.cov <- coredata(ts.irrad.surf[paste(2000+yr,'-01-01/',2000+yr,'-12-31',sep='')])
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
  autoplot(fc.irrad.surf[[yr-3]])
  ggsave(paste('fc.irrad.surf',yr,'.png',sep=''))
  
  # irradiance top of atomosphere as cov
  train.cov <- coredata(ts.irrad.atmo[paste('2003-01-01/',2000+yr-1,'-12-31',sep='')])
  test.cov <- coredata(ts.irrad.atmo[paste(2000+yr,'-01-01/',2000+yr,'-12-31',sep='')])
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
  autoplot(fc.irrad.atmo[[yr-3]])
  ggsave(paste('fc.irrad.atmo',yr,'.png',sep=''))
  
  # snowfall as cov
  train.cov <- coredata(ts.snowf[paste('2003-01-01/',2000+yr-1,'-12-31',sep='')])
  test.cov <- coredata(ts.snowf[paste(2000+yr,'-01-01/',2000+yr,'-12-31',sep='')])
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
  autoplot(fc.snowf[[yr-3]])
  ggsave(paste('fc.snowf',yr,'.png',sep=''))
  
  # snow depth as cov
  train.cov <- coredata(ts.snowd[paste('2003-01-01/',2000+yr-1,'-12-31',sep='')])
  test.cov <- coredata(ts.snowd[paste(2000+yr,'-01-01/',2000+yr,'-12-31',sep='')])
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
  autoplot(fc.snowd[[yr-3]])
  ggsave(paste('fc.snowd',yr,'.png',sep=''))
  
  # cloud cover as cov
  train.cov <- coredata(ts.cloud[paste('2003-01-01/',2000+yr-1,'-12-31',sep='')])
  test.cov <- coredata(ts.cloud[paste(2000+yr,'-01-01/',2000+yr,'-12-31',sep='')])
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
  autoplot(fc.cloud[[yr-3]])
  ggsave(paste('fc.cloud',yr,'.png',sep=''))
  
  # air density as cov
  train.cov <- coredata(ts.airden[paste('2003-01-01/',2000+yr-1,'-12-31',sep='')])
  test.cov <- coredata(ts.airden[paste(2000+yr,'-01-01/',2000+yr,'-12-31',sep='')])
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
  autoplot(fc.airden[[yr-3]])
  ggsave(paste('fc.airden',yr,'.png',sep=''))
  
  # temperature + irradiance at surface as cov
  train.cov <- cbind(coredata(ts.temp[paste('2003-01-01/',2000+yr-1,'-12-31',sep='')]),
                     coredata(ts.irrad.surf[paste('2003-01-01/',2000+yr-1,'-12-31',sep='')]))
  test.cov <- cbind(coredata(ts.temp[paste(2000+yr,'-01-01/',2000+yr,'-12-31',sep='')]),
                    coredata(ts.irrad.surf[paste(2000+yr,'-01-01/',2000+yr,'-12-31',sep='')]))
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
  autoplot(fc.cov1[[yr-3]])
  ggsave(paste('fc.cov1',yr,'.png',sep=''))
  
  # temperature + snow depth as cov
  train.cov <- cbind(coredata(ts.temp[paste('2003-01-01/',2000+yr-1,'-12-31',sep='')]),
                     coredata(ts.snowd[paste('2003-01-01/',2000+yr-1,'-12-31',sep='')]))
  test.cov <- cbind(coredata(ts.temp[paste(2000+yr,'-01-01/',2000+yr,'-12-31',sep='')]),
                    coredata(ts.snowd[paste(2000+yr,'-01-01/',2000+yr,'-12-31',sep='')]))
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
  autoplot(fc.cov2[[yr-3]])
  ggsave(paste('fc.cov2',yr,'.png',sep=''))
  
  # snow depth + irradiance at surface as cov
  train.cov <- cbind(coredata(ts.snowd[paste('2003-01-01/',2000+yr-1,'-12-31',sep='')]),
                     coredata(ts.irrad.surf[paste('2003-01-01/',2000+yr-1,'-12-31',sep='')]))
  test.cov <- cbind(coredata(ts.snowd[paste(2000+yr,'-01-01/',2000+yr,'-12-31',sep='')]),
                    coredata(ts.irrad.surf[paste(2000+yr,'-01-01/',2000+yr,'-12-31',sep='')]))
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
  autoplot(fc.cov3[[yr-3]])
  ggsave(paste('fc.cov3',yr,'.png',sep=''))
  
  # temperature + irradiance at surface + snow depth as cov
  train.cov <- cbind(coredata(ts.temp[paste('2003-01-01/',2000+yr-1,'-12-31',sep='')]),
                     coredata(ts.irrad.surf[paste('2003-01-01/',2000+yr-1,'-12-31',sep='')]),
                     coredata(ts.snowd[paste('2003-01-01/',2000+yr-1,'-12-31',sep='')]))
  test.cov <- cbind(coredata(ts.temp[paste(2000+yr,'-01-01/',2000+yr,'-12-31',sep='')]),
                    coredata(ts.irrad.surf[paste(2000+yr,'-01-01/',2000+yr,'-12-31',sep='')]),
                    coredata(ts.snowd[paste(2000+yr,'-01-01/',2000+yr,'-12-31',sep='')]))
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
  autoplot(fc.cov4[[yr-3]])
  ggsave(paste('fc.cov4',yr,'.png',sep=''))
  stopImplicitCluster()
}
