
setwd("C:/Users/sharon.liu/Documents/R/course 5/")
outdir<- getwd()
if (!require("ggplot2")) {
  install.packages("ggplot2", repos="http://cran.rstudio.com/") 
  library("ggplot2")
}
if (!require("scales")) {
  install.packages("scales", repos="http://cran.rstudio.com/") 
  library("scales")
}
if (!require("Hmisc")) {
  install.packages("Hmisc", repos="http://cran.rstudio.com/") 
  library("Hmisc")
}

steps<-read.csv("activity.csv",header=TRUE, sep=",")
steps_notna<-subset(steps, !is.na(steps))


steps_day<-aggregate(steps[1],by=steps[2],FUN=sum, na.rm=TRUE)

hist(steps_day$steps,breaks=20, col="yellow", main="Total Number of Steps Per day", xlab="Steps Per Day", border="blue")

mean_steps<-mean(steps_day$steps)
mean_steps

med_steps<-median(steps_day$steps)
med_steps

step_5min<-aggregate(steps~interval, data=steps_notna, mean)

plot(step_5min$interval,step_5min$steps, type="l", main="Average steps per 5-minute interval",ylab="Number of Steps",xlab="Interval")

step_5min$interval[which.max(step_5min$steps)]

  steps_na<-which(is.na(steps))
  length(steps_na)


steps_filled<-merge(steps, step_5min,by="interval")

  steps_na<-which(is.na(steps_filled$steps.x))
  steps_filled$steps.x[steps_na]<-steps_filled$steps.y[steps_na]

steps_day_new<-aggregate(steps.x~date, data=steps_filled, sum)
hist(steps_day_new$steps.x, breaks=20,main="Total number of Steps Per day", xlab="Steps Per Day", border="black", col="yellow")

mean_steps<-mean(steps_day_new$steps.x)
mean_steps

med_steps<-median(steps_day_new$steps.x)
med_steps


ADI <- steps
ADI$steps <- impute(steps$steps, fun=mean)

stepsByDayImputed <- tapply(ADI$steps, ADI$date, sum)

stepsByDayMeanImputed <- mean(stepsByDayImputed)
stepsByDayMedianImputed <- median(stepsByDayImputed)

ADI$dateType <-  ifelse(as.POSIXlt(ADI$date)$wday %in% c(0,6), 'weekend', 'weekday')


averagedADI <- aggregate(steps ~ interval + dateType, data=ADI, mean)
library(ggplot2)
ggplot(averagedADI, aes(interval, steps)) + 
    geom_line() + 
    facet_grid(dateType ~ .) +
    xlab("5-minute interval") + 
    ylab("avarage number of steps")

