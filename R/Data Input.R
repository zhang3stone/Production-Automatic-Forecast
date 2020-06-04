## ------- Libary Requirement ------ ##
library(plyr)
library(ggplot2)
library(scales)
library(reshape)
#Parameter Setting
source(file="./R/Parameters Input.R")
# Data reshape
data<-read.csv("./Data/TMS Input 201612.csv")
regressor<-read.csv("./Data/Regressor.csv")
pricenocute<-read.csv("./Data/PRICENOCUT.CSV")
#pricecute<-read.csv("./Data/PRICECUT.CSV")

source(file="./R/Source Functions.R")

# Date format&reshape
data$Date <- as.POSIXct(as.numeric(as.POSIXct(data$Date, format = timeformat, tz="GMT")), tz="GMT", origin = "1970-01-01")
regressor$Date <- as.POSIXct(as.numeric(as.POSIXct(regressor$Date, format = timeformat, tz="GMT")), tz="GMT", origin = "1970-01-01")
data <- join(data, regressor, by=c("Date"), type="right")

#pricenocute.re<-melt(pricenocute, id.var="SKU")
#pricecute.re<-melt(pricecute, id.var="SKU")

#pricenocute.re<-reshape::rename(pricenocute.re, c(SKU="Package",variable="Period",value="Price.SC1"))
#pricecute.re<-reshape::rename(pricecute.re, c(SKU="Package",variable="Period",value="Price.SC2"))

#pricenocute.re$Date<-as.Date(paste(substr(pricenocute.re$Period,6,7),"01", substr(pricenocute.re$Period,2,5),sep="/"), "%m/%d/%Y")
#pricecute.re$Date<-as.Date(paste(substr(pricenocute.re$Period,6,7),"01",substr(pricenocute.re$Period,2,5),sep="/"), "%m/%d/%Y")

#pricenocute.re$Month<-NULL
#pricecute.re$Month<-NULL

# Data Filter
if(!is.null(filter.BU)) data<-subset(data, BU %in% filter.BU)
if(!is.null(filter.Brand)) data<-subset(data, Brand %in% filter.Brand)
if(!is.null(filter.Package)) data<-subset(data, Package %in% filter.Package)

# Data Aggregation
eval(parse(text=paste("data<-aggregate(Volume ~ Date + ", as.name(paste(group.by,collapse="+")), ", data, FUN=sum)",sep="")))

# Business Rules Check
judge.all <- eval(parse(text=paste("ddply(data, .(", paste(group.by,collapse=",") ,"), businessrule)")))
data <- join(data, judge.all, by = group.by, type = "inner")
data <- subset(data, judge %in% c(0))

data.good <- subset(data, as.Date(data$Date) >= date.start & as.Date(data$Date) <=date.latest)
