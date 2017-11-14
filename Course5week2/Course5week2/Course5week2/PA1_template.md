---
title: "Reproducible_Research_WK2"
author: "Reed"
date: "Nov 2017"
output: html_document
  keep_md: true 
---
#Code chunk for loading libraries

library(readr)
library(dplyr)
## 
## Attaching package: 'dplyr'
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
library(lubridate)
## 
## Attaching package: 'lubridate'
## The following object is masked from 'package:base':
## 
##     date
library(ggplot2)
#Code chunk for reading in the data

#work
#activity <- read_csv("C:/Users/reed/Documents/My Dropbox/Dani life/2017 Coursera/Course 5 - Reproducible/Wk2/activity.csv")
#home
activity <- read_csv("C:/Users/Reed/Dropbox/Dani life/2017 Coursera/Course 5 - Reproducible/Wk2/activity.csv")
## Parsed with column specification:
## cols(
##   steps = col_integer(),
##   date = col_date(format = ""),
##   interval = col_integer()
## )
#Code chunk for histogram of the total number of steps taken each day

total_number_daily_steps <- aggregate(steps~date, data=activity, na.remove = TRUE, FUN=sum)
hist(total_number_daily_steps$steps)
plot of chunk unnamed-chunk-2 #Code chunk for the mean and median number of steps taken each day

x <- mean(total_number_daily_steps$steps)
x
## [1] 10767.19
y <- median(total_number_daily_steps$steps)
y
## [1] 10766
#First answer: the mean total numberof steps taken each day is 1.0767189 × 104 and the median is 10766.

#Time series plot of the average number of steps taken

time_series <- tapply(activity$steps, activity$interval, mean, na.rm = TRUE)
plot(row.names(time_series), time_series, type = "l", xlab = "Five minute intervals", ylab = "Average for days", main = "Average N of steps")
plot of chunk unnamed-chunk-4 #Code chunk for the 5-minute interval that, on average, contains the maximum number of steps

max <- as.data.frame(time_series)
max1 <- max(max$time_series, na.rm = TRUE)
max2<- which(max$time_series==max1, arr.ind=T)
z <-row.names(max2) 
#The five minute interval that, on average, contains the maximum number of steps is 835.

#Code to describe and show a strategy for imputing missing data (part 1)

na_count <-sapply(activity, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
na_count
##          na_count
## steps        2304
## date            0
## interval        0
#Code to describe and show a strategy for imputing missing data (part 2)

activity1 <- activity
activity1$steps[is.na(activity1$steps)] <- 0
#Histogram of the total number of steps taken each day after missing values are imputed

total_number_daily_steps1 <- aggregate(steps~date, data=activity1, na.remove = TRUE, FUN=sum)
hist(total_number_daily_steps1$steps)
![plot of chunk hist with missing](figure/hist with missing-1.png)

summary(total_number_daily_steps1$steps)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       1    6779   10396    9355   12812   21195
#What is the impact of imputing missing data on the estimates of the total daily number of steps? #The mean number of steps is 835 when missing data are coded as zero vs. 1.0767189 × 104 when missing data are ignored.

#Do these values differ from the estimates from the first part of the assignment? #Yes, when we replace NA with zeros, it reduces the mean steps per day by about 200.

#Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

#create a vector of weekdays and make a new factor weekend vs weekday
weekdays1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
activity1$week <- factor((weekdays(activity1$date) %in% weekdays1), levels=c(FALSE, TRUE), labels=c('weekend','weekday')) 
#now make a weekday and a weekend file
weekend <- activity1[activity1$week == "weekend",]
weekday <- activity1[activity1$week == "weekday",]

par(mfcol = c(2, 1))

time_series1 <- tapply(weekend$steps, weekend$interval, mean, na.rm = TRUE)
plot(row.names(time_series1), time_series1, type = "l", xlab = "Military time but with no semi colon to separate hours and minutes", 
    ylab = "Weekend", main = "Average N of steps")

time_series2 <- tapply(weekday$steps, weekday$interval, mean, na.rm = TRUE)
plot(row.names(time_series2), time_series2, type = "l", xlab = "Military time but with no semicolon to separate hours and minutes", 
    ylab = "Weekdays")
plot of chunk weekdays
© 2017 GitHub, Inc.
Terms
Privacy
Security
Status
Help
Contact GitHub
API
Training
Shop
Blog
