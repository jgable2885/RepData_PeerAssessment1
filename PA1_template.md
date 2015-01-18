---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
First, read in the data.


```r
unzip("activity.zip")
actData <- read.csv("activity.csv")
head(actData)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```


## What is mean total number of steps taken per day?

Compute the total steps taken per day and remove missing values. This uses the dplyr package.


```r
library(dplyr)
byDay <- group_by(actData,date)
totalStepsByDay <- summarise(byDay, total=sum(steps, na.rm=TRUE))
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

The mean total per day (dotted line) is 5.70608 &times; 10<sup>5</sup> steps and the median (long-dash line) is 570608 steps per day.


## What is the average daily activity pattern?

Compute the average steps per time interval across all days and plot it as a time series.


```r
byInterval <- group_by(actData, interval)
meanByInterval <- summarise(byInterval, mean=mean(steps,na.rm=TRUE))
plot(meanByInterval$interval,meanByInterval$mean, type="l", xlab="Interval", ylab="Mean steps")
```

```
## Error in xy.coords(x, y, xlabel, ylabel, log): 'x' and 'y' lengths differ
```

```r
maxAvgSteps <- max(meanByInterval$mean)
roundedMax <- round(maxAvgSteps, 2)
maxAvgStepsInt <- meanByInterval[[which.max(meanByInterval$mean),1]]
```

Interval 37.3825996 had the most steps on average, with 37.38 steps over those five minutes.


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
tempByDay <- group_by(temp,date)
missingByDay <- summarise(tempByDay,totalMissing = sum(missing))
library(xtable)
xt <- xtable(data.frame(missingByDay))
print(xt,type="html")
```

<!-- html table generated in R 3.1.1 by xtable 1.7-4 package -->
<!-- Sun Jan 18 01:44:18 2015 -->
<table border=1>
<tr> <th>  </th> <th> totalMissing </th>  </tr>
  <tr> <td align="right"> 1 </td> <td align="right"> 2304.00 </td> </tr>
   </table>

In order to devise a strategy for imputing the missing values, first look to see how the total steps varies by day throughout the 2-month period.


```r
plot(as.Date(totalStepsByDay$date),totalStepsByDay$total, type="o", xlab="Date", ylab="Total steps per day")
```

```
## Error in as.Date.default(totalStepsByDay$date): do not know how to convert 'totalStepsByDay$date' to class "Date"
```

From the above plot it is apparent that other than the days with no data and a few days with very few steps, the total number of steps per day is somewhat uniform. For this reason, it seems reasonable as a first approximation to assign all days with no data means based on the average number of steps per interval. This makes all missing days into "average" days.


```r
actDataImpute <- actData
daysToImpute <- as.list(missingByDay[missingByDay$totalMissing==288,1])
for (day in daysToImpute[[1]]){
        #since all days have the same intervals each day that is missing 
        #values can be directly replaced with an average day (without worrying
        #about matching intervals explictly)
        actDataImpute[as.character(actDataImpute$date)==day,1] <- meanByInterval$mean
}
```

```
## Error in daysToImpute[[1]]: subscript out of bounds
```

Let's check how that same plot looks now that missing values have been imputed.


```r
byDayImpute <- group_by(actDataImpute,date)
totalStepsByDayImpute <- summarise(byDayImpute, total=sum(steps, na.rm=TRUE))
plot(as.Date(totalStepsByDayImpute$date),totalStepsByDayImpute$total, type="o", xlab="Date", ylab="Total steps by day")
```

```
## Error in as.Date.default(totalStepsByDayImpute$date): do not know how to convert 'totalStepsByDayImpute$date' to class "Date"
```


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

With the imputed values, the mean and median are now equal (570608 and 570608 steps, respectively). Both the mean and median increased relative to their previous values. The mean was 5.70608 &times; 10<sup>5</sup> steps before imputation and 570608 steps after. Likewise the median was 570608 steps before and 570608 steps after.


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
library(plyr)
actDataImputeWeekend <- actDataImpute[actDataImpute$weekend == "weekend",]
actDataImputeWeekday <- actDataImpute[actDataImpute$weekend == "weekday",]
meanWeekend <- ddply(actDataImputeWeekend, .(interval), summarise, mean=mean(steps,na.rm=TRUE))
meanWeekday <- ddply(actDataImputeWeekday, .(interval), summarise, mean=mean(steps,na.rm=TRUE))
par(mfrow=c(2,1), mar=c(4,4,2,2))
plot(meanWeekday$interval,meanWeekday$mean,"l", ylim=c(0,240), ylab="Weekday mean steps", xlab="Interval", main="Mean steps per interval for weekday (top) and weekend (bottom) days")
plot(meanWeekend$interval,meanWeekend$mean,"l", ylim=c(0,240), ylab="Weekend mean steps", xlab="Interval")
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png) 

The weekday average has a large peak at interval 835 that is not present during the weekend. In addition, weekday steps start just after interval 500 while a substantial increase is delayed for weekends till approximately interval 800.  

