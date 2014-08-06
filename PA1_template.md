# Reproducible Research: Peer Assessment 1
name: Zeng Mingwei  
email: 341445826@qq.com

## Loading and preprocessing the data


```r
data <- read.csv('activity.csv')
```


## What is mean total number of steps taken per day?

1. Make a histogram of the total number of steps taken each day


```r
steps.eachday <- with(data,tapply(steps,date,sum))
barplot(steps.eachday,xlab='date',ylab='total number of steps',main='Total number of steps taken each day')
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1.png) 

2. Calculate and report the mean and median total number of steps taken per day


```r
mean(steps.eachday,na.rm=T)
```

```
## [1] 10766
```

```r
median(steps.eachday,na.rm=T)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
avgSteps.eachInterval <- with(data,tapply(steps,interval,mean,na.rm=T))
plot(avgSteps.eachInterval,type='l',xlab='interval',ylab='average steps',main='Average number of steps taken')
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
maxAvgSteps.Interval <- which(avgSteps.eachInterval==max(avgSteps.eachInterval),arr.ind=T)
rownames(maxAvgSteps.Interval)
```

```
## [1] "835"
```

## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
sum(is.na(data$steps))
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

I use the mean for that 5-minute interval to fill in the missing values.


```r
library(Hmisc)
times <- sum(is.na(data$steps)) / length(avgSteps.eachInterval)
imputation <- rep(avgSteps.eachInterval,times)
steps <- impute(data$steps,imputation)
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
data.imputed <- cbind(steps,data[,c(2,3)])
```

   Number of missing value in data.imputed:


```r
sum(is.na(data.imputed))
```

```
## [1] 0
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
steps.eachday <- with(data.imputed,tapply(steps,date,sum))
barplot(steps.eachday,xlab='date',ylab='total number of steps',main='Total number of steps taken each day')
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9.png) 


```r
mean(steps.eachday)
```

```
## [1] 10766
```

```r
median(steps.eachday)
```

```
## [1] 10766
```

We see that the mean total number of steps taken per day is the same as the mean in the first part of the assignment. And the median is slightly different, which shows that the impact of imputing missing data is neglectable.

## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
date <- as.Date(data.imputed$date,'%Y-%m-%d')
weekday <- as.factor(weekdays(date))
levels(weekday) <- c('Weekday','Weekday','Weekend','Weekend','Weekday','Weekday','Weekday')
data.imputed <- cbind(data.imputed,weekday)
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
library(reshape2)
library(lattice)
avgSteps.eachInterval <- with(data.imputed,tapply(steps,list(interval,weekday),mean,na.rm=T))
melt.data <- melt(avgSteps.eachInterval)
names(melt.data) <- c('interval','weekday','avgSteps')
xyplot(avgSteps ~ interval | weekday, melt.data, type='l', layout=c(1,2),ylab='average steps')
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12.png) 
