# Libary Requirement
library(plyr)
library(forecast)
library(reshape2)
library(sqldf)

source(file="./R/Function_Model_Forecasting.R")


metrics<-metrics.summary
method<-eval(parse(text=paste("sqldf('select", paste(group.by,collapse=","), ", Method, min(MASE) from metrics group by", paste(group.by,collapse=","),"')",collapse="")))
data.good<-join(data.good,method, by=group.by)

data.f1<-subset(data.good, Method %in% c("STL_ETS"))
data.f2<-subset(data.good, Method %in% c("STL_snaive"))
data.f3<-subset(data.good, Method %in% c("STL_ARIMA"))
data.f4<-subset(data.good, Method %in% c("STL_REG"))


output.ets   <-eval(parse(text=paste("ddply(data.f1, .(", as.name(paste(group.by,collapse=",")), "), forecast.result, method='STL_ETS')",sep="")))
output.snaive<-eval(parse(text=paste("ddply(data.f2, .(", as.name(paste(group.by,collapse=",")), "), forecast.result, method='STL_snaive')",sep="")))
output.arima <-eval(parse(text=paste("ddply(data.f3, .(", as.name(paste(group.by,collapse=",")), "), forecast.result, method='STL_ARIMA')",sep="")))
output.reg   <-eval(parse(text=paste("ddply(data.f4, .(", as.name(paste(group.by,collapse=",")), "), forecast.result, method='STL_REG')",sep="")))

output.all<-rbind(output.ets,output.snaive,output.arima,output.reg)
#Add Price

#Price.table <- join(pricecute.re, pricenocute.re, by=c("Date","Package"))
#select.all <- join(output.all, Price.table, by=c("Date","Package"), type="inner")
#select.all$Period <- NULL 

#select.all$Value.SC1<-select.all$forecast*select.all$Price.SC1
#select.all$Value.SC2<-select.all$forecast*select.all$Price.SC2

#select.all$lo95<-NULL
#select.all$hi95<-NULL
#select.all$lo80<-NULL
#select.all$hi80<-NULL
#Split By PL;

w<-function(data){
suffix<-data$PL[1]
eval(parse(text=paste("write.csv(data, './Output/",suffix,"_FORECAST.csv')",sep="")))
}

#output.log<-by(select.all, select.all$PL, w)

write.csv(output.all, "./Output/CIP_FORECAST.csv")