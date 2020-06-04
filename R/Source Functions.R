

businessrule <- function(data){
# Apply business rules

  tsVolume <- data$Volume
  tsDate <- data$Date
  
  # Rule 1: if a time series has no more than <min.length> non-NA values, discard
  if (sum(!is.na(tsVolume)) < min.length) return(c(judge = 1))
  
  # Rule 2: if a time series has any sales quantity <= value.threshold , discard
  #if (length(tsVolume[tsVolume > value.threshold]) != length(tsVolume)) return(c(judge = 0))
  
  # Rule 3: if the latest transection time <= data.threshold, discard
  if (length(tsDate[as.Date(tsDate) > date.threshold]) <1) return(c(judge = 1))
  
  return(c(judge = 0))
}

growth<-function (data) {
# Calculate Growth
  data$Growth <- c(NA, tail(data$Value, -1) / head(data$Value, -1) - 1)
  data
}

cagr<-function (data) {
# Calculate CAGR
  cagr <- (tail(data$Value,1) / head(data$Value,1)) ^ (1/nrow(na.omit(data)))-1
  return(cagr)
}


MAT<-function(data) {
# Calculate MAT

  data$MAT<-ifelse (as.numeric(substr(data$Date,6,7))<=as.numeric(substr(date.latest,6,7)),paste("MAT",as.numeric(substr(data$Date,1,4)), substr(date.latest,6,7), sep=""), paste("MAT",as.numeric(substr(data$Date,1,4))+1, substr(date.latest,6,7), sep=""))
  return(data)
}

YTD<-function(data) {
  # Calculate YTD
  
  data$YTD<-ifelse (as.numeric(substr(data$Date,6,7))<=as.numeric(substr(date.latest,6,7)),paste(as.numeric(substr(data$Date,1,4)), substr(date.latest,6,7), sep=""),"")
  return(data)
}

group_midpoints = function(g) {
  
  cumsums = c(0, cumsum(g$Value))
  diffs = diff(cumsums)
  pos = head(cumsums, -1) + (0.5 * diffs)
  return(data.frame(Brand=g$Brand, Pos=pos))
}

# Helper functions extracting date-related information
year <- function(date){ date <- as.Date(date, format=timeformat); as.numeric(format(date, "%Y"))}
weeknum <- function(date){ date <- as.Date(date, format=timeformat); as.numeric(format(date, "%m"))}
date.info <- function(df){ date <- df$Date[1]; c(year(date), weeknum(date))}
