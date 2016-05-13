## download and load the dataset to act
setwd('C:/Courses/coursera/05 Reproducible research/assignment')
filename <- "Activity monitoring data.zip"
if (!file.exists(filename)){
        fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip "
        download.file(fileURL, filename, mode = 'wb')
}  
if (!file.exists("Activity monitoring data")) { 
        unzip(filename) 
}
act <- read.csv('activity.csv')

## What is mean total number of steps taken per day?
## Calculate the total number of steps taken per day
TotalStepByDay <- with(act, tapply(steps, date, sum, na.rm = TRUE))

## Make a histogram of the total number of steps taken each day
hist(TotalStepByDay, breaks = 10, xlab = 'Steps number', 
     main = 'Total number of steps taken each day')
dev.copy(png, 'TotalStepsPerDay.png')
dev.off()

## Calculate report the mean and median of the total number of steps taken per day
Mean1 <- mean(TotalStepByDay)
Median1 <- median(TotalStepByDay) 
## Mean = 9354; Median = 10400

## Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
## and the average number of steps taken, averaged across all days (y-axis)
MeanStepByTime <- with(act1, tapply(steps, interval, mean))
plot(names(MeanStepByTime), MeanStepByTime, type = 'l', ylab = 'Steps number', xlab = 'Time interval', 
     main = 'Total number of steps taken each day')
dev.copy(png, 'MeanStepsPerInterval.png')
dev.off()

## Which 5-minute interval, on average across all the days in the dataset, 
## contains the maximum number of steps?
names(which.max(MeanStepByTime))

## Calculate and report the total number of missing values in the dataset
sum(is.na(act))

## Filling in all of the missing values in the dataset by the mean for that 5-minute interval
act[is.na(act), 1] <- MeanStepByTime[as.character(act[is.na(act),3])]

## Make a histogram of the total number of steps taken each day and 
## Calculate and report the mean and median total number of steps taken per day. 
TotalStepByDay2 <- with(act, tapply(steps, date, sum))
hist(TotalStepByDay2, breaks = 10, xlab = 'Steps number', 
     main = 'Total number of steps taken each day without missing values')
dev.copy(png, 'TotalStepsPerDay2.png')
dev.off()
Mean2 <- mean(TotalStepByDay2)
Median2 <- median(TotalStepByDay2)

## Do these values differ from the estimates from the first part of the assignment? 
## What is the impact of imputing missing data on the estimates of the total daily number of steps?

## Are there differences in activity patterns between weekdays and weekends?
## Create a new factor variable in the dataset with two levels - "weekday" and "weekend" 
## indicating whether a given date is a weekday or weekend day.
week <- weekdays(as.Date(act$date, '%Y-%m-%d'))
weekend <- c('Saturday', 'Sunday')
act$date <- factor((week %in% weekend), labels = c('weekday', 'weekend'))

## Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
## and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 
library(lattice)
StepsByWeek <- with(act, aggregate(steps, by = list(interval, date), FUN = mean))
xyplot(x ~ Group.1 | Group.2, StepsByWeek, type = 'l', ylab = 'Steps number', xlab = 'Time interval', 
     main = 'Total number of steps taken on weekday and weekend', 
     layout = c(2, 1))
dev.copy(png, 'TotalStepsWeek.png')
dev.off()
