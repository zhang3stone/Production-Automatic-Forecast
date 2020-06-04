## ------- User-Defined Parameters ------ ##

# Setup working directory
setwd("C:/Works/Francis R Repository/Production Automatic Forecast")

# Data Parameters
filter.BU<-NULL
filter.Brand<-NULL
filter.Package<-NULL
group.by<-c("BU","PL","Brand")

# Forecast Parameters
forecast.length <- 6
test.length <- 6
# seasonal cycle
seasonality <- 12
# time unit of observation
observation.freq <- "Month"
timeformat <- "%Y/%m/%d"

min.length <- 3*seasonality
value.threshold <- 0
date.threshold<-as.Date("2012-12-01")
date.latest<-as.Date("2016-06-01")
date.start<-as.Date("2012-12-01")
# start point that doestn't involve abnormal trend
date.start.correct<-as.Date('2013-01-01')

# Parameters Table
input<- data.frame(test.length = test.length, seasonality = seasonality, 
                   observation.freq = observation.freq, timeformat = timeformat,
                   stringsAsFactors = FALSE
                   )

