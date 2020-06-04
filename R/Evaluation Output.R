## ------- Model Evaluation ------ ##

library(forecast)
library(plyr)
library(reshape2)

source(file="./R/Function_Model_Trainning.R")

output.stlets<-eval(parse(text=paste("ddply(data.good, .(", as.name(paste(group.by,collapse=",")), "), stl.single.id, method='STL_ETS')",sep="")))
output.stlarima<-eval(parse(text=paste("ddply(data.good, .(", as.name(paste(group.by,collapse=",")), "), stl.single.id, method='STL_ARIMA')",sep="")))
output.stlrwf<-eval(parse(text=paste("ddply(data.good, .(", as.name(paste(group.by,collapse=",")), "),stl.single.id, method='STL_snaive')",sep="")))
output.stlreg<-eval(parse(text=paste("ddply(data.good, .(", as.name(paste(group.by,collapse=",")), "),stl.single.id, method='STL_REG')",sep="")))

get.test<-function(data){
  # Train and test split
  data.length <- nrow(data)
  train.length <- data.length - horizon
  test <- data[(train.length+1):data.length, ]
  
  return(test)
}

testoutput<-eval(parse(text=paste("ddply(data.good,.(",paste(group.by,collapse=","),"), get.test)")))
testoutput<-merge(testoutput, output.stlets, by=c(group.by,'Date'), all=TRUE)
testoutput<-merge(testoutput, output.stlarima, by=c(group.by,'Date'), all=TRUE )
testoutput<-merge(testoutput, output.stlrwf, by=c(group.by,'Date'), all=TRUE )
testoutput<-merge(testoutput, output.stlreg, by=c(group.by,'Date'), all=TRUE )

# Helper function
extract.col <- function(pattern, df){
  col.indices <- grep(pattern, colnames(df), ignore.case = TRUE)
  return(df[, col.indices, drop = FALSE])
}

# Metric Functions
mase <- function(true, forecast){
  error = 0;
  if (length(true) != length(forecast)) {
    return (NA);
  } else if (length(true) == 0 || length(forecast) == 0) {
    return (NA);
  }
  else {
    denom = (sum(abs(true[2:length(true)] - true[1:(length(true) - 1)])))/(length(true) - 1)
    error = sum((abs(true-forecast)) / denom)/length(true);
  }
  return (error);
}

get.metrics <- function(true, forecast){
  forecast.metrics <- as.data.frame(accuracy(forecast, true))
  return(data.frame(forecast.metrics[, colnames(forecast.metrics) !="ME"], MASE = mase(true, forecast)))
}

get.metrics.single.id <- function(data){
  # Extract forecast values
  data.forecast <- extract.col("forecast\\.[a_z]*", df = data)
  
  # Split true data and forecast values
  is.test <- !is.na(data.forecast[,1])
  
  test.true <- data$Volume[is.test]
  test.forecast <- data.forecast[is.test, ]
  test.time <- data$Date[is.test]
  
  test.nonna <- complete.cases(test.true)
  test.true <- test.true[test.nonna]
  test.forecast <- test.forecast[test.nonna,]
  #test.time <- test.time[test.nonna]
  
  # Calculate error metrics
  output <- t(sapply(test.forecast, get.metrics, true = test.true))
  
  methods <- sub("[a-z]+\\.", "", rownames(output), ignore.case = TRUE)
  output <- apply(output, c(1,2),as.numeric)
  output <- data.frame(test.length.nonna = sum(test.nonna), Method = methods, output)
  rownames(output) <- NULL
  return(output)
}

metrics.all.ids <- eval(parse(text=paste("ddply(testoutput,.(",paste(group.by,collapse=","),"), get.metrics.single.id)")))
metrics.all.ids.reshape<-melt(metrics.all.ids,id=c(group.by,"test.length.nonna","Method"))

get.mean.metric.summary <- function(mean.metric, N){
  return(weighted.mean(mean.metric, N))
}

get.rmse.summary <- function(rmse.metric, N){
  return(c("RMSE" = sqrt(weighted.mean(rmse.metric^2, N))))
}

get.metrics.summary <- function(metrics.all.ids){
  summary.mean <- apply(extract.col("^M[A-Z]+E", metrics.all.ids), MARGIN = 2, 
                        FUN = get.mean.metric.summary, N = metrics.all.ids$test.length.nonna)
  
  summary.rmse <- get.rmse.summary(metrics.all.ids$RMSE, metrics.all.ids$test.length.nonna)
  return(c(summary.mean, summary.rmse))
}

metrics.summary <- eval(parse(text=paste("ddply(metrics.all.ids,.(",paste(paste(group.by,collapse=","),",Method"),"), get.metrics.summary)")))
metric.names <- grep("^M[A-Z]+E|RMSE", colnames(metrics.all.ids), ignore.case = TRUE, value = TRUE)

bp<-ggplot(data=metrics.all.ids.reshape, aes(x=Method, y=value))+geom_boxplot(fill="#003366",color="black")+facet_wrap(~variable, scale="free_y")
print(bp)
