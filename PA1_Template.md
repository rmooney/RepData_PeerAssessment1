# Reproducible Research: Peer Assessment 1
========================================================
The data for this assignment is in the physical file activity.csv.
This assumes the file is present in the working directory.

First, read in the data.

```r
act <- read.csv("activity.csv")
```


For this part of the assignment, you can ignore the missing values in the dataset.

## What is mean total number of steps taken per day?

*Make a histogram of the total number of steps taken each day*


```r
steps.date <- aggregate(steps ~ date, data=act, FUN=sum)
hist(steps.date$steps,xlab="Steps", breaks=10)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

*Calculate and report the mean and median total number of steps taken per day*


```r
mean(steps.date$steps)
```

```
## [1] 10766
```

```r
median(steps.date$steps)
```

```
## [1] 10765
```
## What is the average daily activity pattern?

*Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)*




```r
steps.interval <- aggregate(steps ~ interval, data=act, FUN=mean)
plot(steps.interval, type="l")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

*Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?*


```r
steps.interval$interval[which.max(steps.interval$steps)]
```

```
## [1] 835
```
## Imputing missing values

*Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with `NA`s)*


```r
sum(is.na(act))
```

```
## [1] 2304
```

*Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.*

**For this assignment, we will use the mean to fill in missing values in the dataset.**

*Create a new dataset that is equal to the original dataset but with the missing data filled in.*


```r
act2 <- merge(act, steps.interval, by="interval", suffixes=c("",".y"))
nas <- is.na(act$steps)
act2$steps[nas] <- act2$steps.y[nas]
act2 <- act2[,c(1:3)]
```

*Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?*


```r
steps2.date <- aggregate(steps ~ date, data=act2, FUN=sum)
hist(steps2.date$steps,xlab="Steps", breaks=10)
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 

```r
mean(steps2.date$steps)
```

```
## [1] 9564
```

```r
median(steps2.date$steps)
```

```
## [1] 11216
```

**The histogram, mean and median values do change with the fillers.**

## Are there differences in activity patterns between weekdays and weekends?

*Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.*


```r
daytype <- function(date) {
  if (weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) {
    "weekend"
    }
  else {
    "weekday"
    }
}

act2$daytype <- as.factor(sapply(act2$date, daytype))
```

*Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).*


```r
par(mfrow=c(2,1))

for (type in c("weekend", "weekday")) {
    steps2.type <- aggregate(steps ~ interval,
                             data=act2,
                             subset=act2$daytype==type,
                             FUN=mean)
    
    plot(steps2.type, type="l", main=type)
}
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 

**The weekend days appear to have a higher number of steps on average.**
