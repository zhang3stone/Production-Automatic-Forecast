# Libary Requirement
library(plyr)
library(forecast)
library(reshape2)

 forecast.result<-function(data, method){

  # Missing data: replace na with average
  data$Volume[is.na(data$Volume)] <- mean(data$Volume, na.rm = TRUE)
  
  # Build forecasting models
  data.ts <- ts(data$Volume, frequency = seasonality, start = date.info(data))
  data.stl <- stl(data.ts, s.window="periodic")
  
  m <- frequency(data.stl$time.series)
  n <- nrow(data.stl$time.series)
  h <- horizon
  
  lastseas <- rep(data.stl$time.series[n - (m:1) + 1, "seasonal"], trunc(1 + (h - 1)/m))[1:h]
  
  start<-which(as.Date(yearmon(index(as.ts(seasadj(data.stl))))) == date.start.correct) + 1
  len<-length(as.Date(yearmon(index(as.ts(seasadj(data.stl))))))
  
  z<-as.zoo(seasadj(data.stl))
  sa <- as.ts(z[start:len])
  
  if (method != "STL_REG") {
    
    if(method == "STL_ETS") {
      fcast <- forecast(ets(sa,model="ZZN",damped=NULL), h = forecast.length, level = c(80, 95) ,ic='bic', opt.crit='mae')
    }
    else if(method == "STL_ARIMA") { 
      fcast <- forecast(auto.arima(sa), h = forecast.length, level = c(80, 95))
    }
    else if(method == "STL_snaive") {
      fcast <- rwf(sa, h=forecast.length, drift=TRUE)
    }
    
    fcast$mean <- fcast$mean + lastseas
    fcast$upper <- fcast$upper + lastseas
    fcast$lower <- fcast$lower + lastseas

    output <- data.frame(Date=as.Date(time(fcast$mean)), cbind( fcast$mean, fcast$upper,fcast$lower))
    colnames(output)[-1] <- paste(c("forecast", "hi80", "hi95", "lo80", "lo95"), sep = ".") 
  }
  
  else if (method == "STL_REG") {
    sad <- data.frame(Date=as.POSIXct(yearmon(index(sa)), format = timeformat, tz="GMT"),  Volume=coredata(sa))
    reg <- subset(regressor,(regressor$Date >= as.POSIXct(min(yearmon(index(sa)))) & regressor$Date <= as.POSIXct(max(yearmon(index(sa))))))
    fcast <- subset(regressor,(regressor$Date > as.POSIXct(max(yearmon(index(sa)))) & regressor$Date <= as.POSIXct(max(yearmon(index(sa)+2)))))
    fit.lm <- step(lm(Volume~Trend+FactorA, data=join(sad, reg, by=c("Date"), type="right")),direction="backward")
    
    fcast$forecast<-predict.lm(fit.lm,newdata=fcast) + lastseas
    fcast$lo80<-predict.lm(fit.lm,newdata=fcast, interval="confidence", level=.8)[,'lwr'] + lastseas
    fcast$hi80<-predict.lm(fit.lm,newdata=fcast, interval="confidence", level=.8)[,'upr'] + lastseas
    fcast$lo95<-predict.lm(fit.lm,newdata=fcast, interval="confidence", level=.95)[,'lwr'] + lastseas
    fcast$hi95<-predict.lm(fit.lm,newdata=fcast, interval="confidence", level=.95)[,'upr'] + lastseas
    fcast$FactorA<-NULL
    fcast$Trend<-NULL
    colnames(fcast)[-1] <- paste(c("forecast", "lo80", "hi80", "lo95", "hi95"), sep = ".") 
    output <- fcast
    
  }
  
  forecast.result <- output
  forecast.result$Method <- method
  return(forecast.result)
}
