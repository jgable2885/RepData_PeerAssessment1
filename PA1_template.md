---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
First, read in the data and convert the interval to a POSIXct time.


```r
unzip("activity.zip")
actData <- read.csv("activity.csv")
actData$IntervalNumber <- actData$interval
#first get all the intervals to have 4 digits with leading zeros as necessary
actData$interval <- formatC(actData$interval, width=4, format="d", flag="0")
#now convert them to times
actData$interval <- as.POSIXlt(strptime(paste(actData$date, actData$interval), "%Y-%m-%d %H%M"))
#now strip away the date by converting to an ITime from the data.table package
library(data.table)
actData$interval <- as.ITime(actData$interval)
head(actData)
```

```
##   steps       date interval IntervalNumber
## 1    NA 2012-10-01 00:00:00              0
## 2    NA 2012-10-01 00:05:00              5
## 3    NA 2012-10-01 00:10:00             10
## 4    NA 2012-10-01 00:15:00             15
## 5    NA 2012-10-01 00:20:00             20
## 6    NA 2012-10-01 00:25:00             25
```


## What is mean total number of steps taken per day?

Compute the total steps taken per day and remove missing values.


```r
library(plyr)
totalStepsByDay <- ddply(actData, .(date), summarize, total=sum(steps, na.rm=TRUE))
```

Plot a histogram of total steps per day and compute the mean and median total steps per day.


```r
hist(totalStepsByDay$total, main="Histogram of total steps per day",xlab="Total steps per day")
meanTotal <- mean(totalStepsByDay$total)
roundedMeanTotal <- round(meanTotal)
medianTotal <- median(totalStepsByDay$total)
abline(v=meanTotal, lty=3, lwd=3)
abline(v=medianTotal, lty=5, lwd=3)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

The mean total per day (dotted line) is 9354 steps and the median (long-dash line) is 10395 steps per day.


## What is the average daily activity pattern?

Compute the average steps per time interval across all days and plot it as a time series.


```r
meanByInterval <- ddply(actData, .(interval), summarize, mean=mean(steps,na.rm=TRUE))
plot(meanByInterval$interval,meanByInterval$mean, type="l", xlab="Time since midnight (s)", ylab="Mean steps", main="Average daily activity pattern")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

```r
maxAvgSteps <- max(meanByInterval$mean)
roundedMax <- round(maxAvgSteps, 2)
maxAvgStepsInt <- meanByInterval[[which.max(meanByInterval$mean),1]]
maxAvgStepsIntNum <- actData$IntervalNumber[maxAvgStepsInt/60/5+1]
```

Interval 835 (30900 seconds after midnight) had the most steps on average, with 206.17 steps over those five minutes.


## Imputing missing values

First, see how many missing values are in the data set.

```r
naCount <- sum(is.na(actData$steps))
percentMissing <- round((naCount/dim(actData)[[1]])*100,1)
```
There are 2304 missing values, 13.1% of the total data. Importantly though, they are not evenly distributed across all days. As shown in the table below the following days have no data (288 missing data points) and make up all of the missing values: Oct 1st, Oct 8th, Nov. 1st, Nov. 4th, Nov 9-10th, Nov. 14th, and Nov. 30th.


```r
# Compute the number of missing values per day and display it as a table
temp <- actData
temp$missing <- as.numeric(is.na(actData$steps))
missingByDay <- ddply(temp, .(date), summarize, totalMissing = sum(missing))
library(xtable)
xt <- xtable(data.frame(missingByDay))
print(xt,type="html")
```

<!-- html table generated in R 3.1.1 by xtable 1.7-4 package -->
<!-- Sun Jan 18 12:03:48 2015 -->
<table border=1>
<tr> <th>  </th> <th> date </th> <th> totalMissing </th>  </tr>
  <tr> <td align="right"> 1 </td> <td> 2012-10-01 </td> <td align="right"> 288.00 </td> </tr>
  <tr> <td align="right"> 2 </td> <td> 2012-10-02 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 3 </td> <td> 2012-10-03 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 4 </td> <td> 2012-10-04 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 5 </td> <td> 2012-10-05 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 6 </td> <td> 2012-10-06 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 7 </td> <td> 2012-10-07 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 8 </td> <td> 2012-10-08 </td> <td align="right"> 288.00 </td> </tr>
  <tr> <td align="right"> 9 </td> <td> 2012-10-09 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 10 </td> <td> 2012-10-10 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 11 </td> <td> 2012-10-11 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 12 </td> <td> 2012-10-12 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 13 </td> <td> 2012-10-13 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 14 </td> <td> 2012-10-14 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 15 </td> <td> 2012-10-15 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 16 </td> <td> 2012-10-16 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 17 </td> <td> 2012-10-17 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 18 </td> <td> 2012-10-18 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 19 </td> <td> 2012-10-19 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 20 </td> <td> 2012-10-20 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 21 </td> <td> 2012-10-21 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 22 </td> <td> 2012-10-22 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 23 </td> <td> 2012-10-23 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 24 </td> <td> 2012-10-24 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 25 </td> <td> 2012-10-25 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 26 </td> <td> 2012-10-26 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 27 </td> <td> 2012-10-27 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 28 </td> <td> 2012-10-28 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 29 </td> <td> 2012-10-29 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 30 </td> <td> 2012-10-30 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 31 </td> <td> 2012-10-31 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 32 </td> <td> 2012-11-01 </td> <td align="right"> 288.00 </td> </tr>
  <tr> <td align="right"> 33 </td> <td> 2012-11-02 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 34 </td> <td> 2012-11-03 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 35 </td> <td> 2012-11-04 </td> <td align="right"> 288.00 </td> </tr>
  <tr> <td align="right"> 36 </td> <td> 2012-11-05 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 37 </td> <td> 2012-11-06 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 38 </td> <td> 2012-11-07 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 39 </td> <td> 2012-11-08 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 40 </td> <td> 2012-11-09 </td> <td align="right"> 288.00 </td> </tr>
  <tr> <td align="right"> 41 </td> <td> 2012-11-10 </td> <td align="right"> 288.00 </td> </tr>
  <tr> <td align="right"> 42 </td> <td> 2012-11-11 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 43 </td> <td> 2012-11-12 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 44 </td> <td> 2012-11-13 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 45 </td> <td> 2012-11-14 </td> <td align="right"> 288.00 </td> </tr>
  <tr> <td align="right"> 46 </td> <td> 2012-11-15 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 47 </td> <td> 2012-11-16 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 48 </td> <td> 2012-11-17 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 49 </td> <td> 2012-11-18 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 50 </td> <td> 2012-11-19 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 51 </td> <td> 2012-11-20 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 52 </td> <td> 2012-11-21 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 53 </td> <td> 2012-11-22 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 54 </td> <td> 2012-11-23 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 55 </td> <td> 2012-11-24 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 56 </td> <td> 2012-11-25 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 57 </td> <td> 2012-11-26 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 58 </td> <td> 2012-11-27 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 59 </td> <td> 2012-11-28 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 60 </td> <td> 2012-11-29 </td> <td align="right"> 0.00 </td> </tr>
  <tr> <td align="right"> 61 </td> <td> 2012-11-30 </td> <td align="right"> 288.00 </td> </tr>
   </table>

In order to devise a strategy for imputing the missing values, first look to see how the total steps varies by day throughout the 2-month period.


```r
plot(as.Date(totalStepsByDay$date),totalStepsByDay$total, type="o", xlab="Date", ylab="Total steps per day")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png) 

From the above plot it is apparent that other than the days with no data and a few days with very few steps, the total number of steps per day is somewhat uniform. For this reason, it seems reasonable as a first approximation to assign all days with no data means based on the average number of steps per interval. This makes all missing days into "average" days.


```r
actDataImpute <- actData
daysToImpute <- as.list(missingByDay[missingByDay$totalMissing==288,1])
for (day in daysToImpute){
        #since all days have the same intervals each day that is missing 
        #values can be directly replaced with an average day (without worrying
        #about matching intervals explictly)
        actDataImpute[as.character(actDataImpute$date)==day,1] <- meanByInterval$mean
}
```

Let's check how that same plot looks now that missing values have been imputed.


```r
totalStepsByDayImpute <- ddply(actDataImpute, .(date), summarize, total=sum(steps, na.rm=TRUE))
plot(as.Date(totalStepsByDayImpute$date),totalStepsByDayImpute$total, type="o", xlab="Date", ylab="Total steps by day")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png) 


Now generate a new histogram and re-compute the mean and median with the imputed values present.


```r
hist(totalStepsByDayImpute$total, main="Histogram of total steps per day with imputed data",xlab="Total steps per day")
meanTotalImpute <- mean(totalStepsByDayImpute$total)
roundedMeanTotalImpute <- as.integer(meanTotalImpute)
medianTotalImpute <- median(totalStepsByDayImpute$total)
roundedMedianTotalImpute <- as.integer(medianTotalImpute)
abline(v=meanTotalImpute, lty=3, lwd=3)
abline(v=medianTotalImpute, lty=5, lwd=3)
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 

With the imputed values, the mean and median are now equal (10766 and 10766 steps, respectively). Both the mean and median increased relative to their previous values. The mean was 9354 steps before imputation and 10766 steps after. Likewise the median was 10395 steps before and 10766 steps after.


## Are there differences in activity patterns between weekdays and weekends?

Now, make a factor variable to indicate whether or not each day is a weekend.


```r
isWeekend <- function(x) {
        if(x == "Sunday" | x == "Saturday"){x = 1} 
        else{ x = 0}
}
actDataImpute$weekend <- sapply(weekdays(as.Date(actDataImpute$date)), isWeekend)
actDataImpute$weekend <- factor(actDataImpute$weekend, labels = c("weekday","weekend"))
```

Now compute the average steps per time interval for weekend days and weekdays, then plot them in a column.


```r
actDataImputeWeekend <- actDataImpute[actDataImpute$weekend == "weekend",]
actDataImputeWeekday <- actDataImpute[actDataImpute$weekend == "weekday",]
meanWeekend <- ddply(actDataImputeWeekend, .(interval), summarise, mean=mean(steps,na.rm=TRUE))
meanWeekday <- ddply(actDataImputeWeekday, .(interval), summarise, mean=mean(steps,na.rm=TRUE))
par(mfrow=c(2,1), mar=c(4,4,2,2))
plot(meanWeekday$interval,meanWeekday$mean,"l", ylim=c(0,240), ylab="Weekday mean steps", xlab="Time since midnight (s)", main="Mean steps per interval for weekday (top) and weekend (bottom) days")
plot(meanWeekend$interval,meanWeekend$mean,"l", ylim=c(0,240), ylab="Weekend mean steps", xlab="Time since midnight (s)")
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png) 

The weekday average has a large peak at interval 835 that is not present during the weekend. In addition, weekday steps start just after interval 500 while a substantial increase is delayed for weekends till approximately interval 800. This shows that on average this individual started taking steps earlier on weekdays, and slightly later on weekends.  

