---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data


```r
unzip("activity.zip")
activity <- read.csv("activity.csv", head=TRUE)
```


## What is mean total number of steps taken per day?

1. Calculate the total number of steps taken per day

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
total_steps <- activity %>% 
  group_by(date) %>% 
  summarise(sumSteps = sum(steps, na.rm = TRUE))
```
2. Make a histogram of the total number of steps taken each day

```r
hist(total_steps$sumSteps, col = "blue", main="Total number of steps taken per day", xlab="Steps")
```

![](figure/unnamed-chunk-3-1.png)<!-- -->


3.  Calculate and report the mean and median of the total number of steps taken per day

```r
meanSteps <- mean(total_steps$sumSteps)
meanSteps
```

```
## [1] 9354.23
```

```r
medianSteps <- median(total_steps$sumSteps)
medianSteps
```

```
## [1] 10395
```


## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l"\color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
average_steps <- aggregate(steps~interval, data=activity, mean, na.rm=TRUE) 
plot(steps~interval, data=average_steps, type = "l", col="blue", xlab="Interval", ylab="Steps", main="Average number of steps per intervals")
```

![](figure/unnamed-chunk-6-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
average_steps[which.max(average_steps$steps), ]$interval
```

```
## [1] 835
```

## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NA\color{red}{\verb|NA|}NAs)


```r
totalMissingValues <- sum(is.na(activity$steps))
totalMissingValues
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
strategy <- average_steps$steps[match(activity$interval, average_steps$interval)]
```

4. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
activity_imputed <- activity %>%
  mutate(
    steps = case_when(
      is.na(steps) ~ average_steps$steps[match(activity$interval, average_steps$interval)],      
      TRUE ~ as.numeric(steps)
    ))
```

5. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
total_steps_imputed <- activity_imputed %>% 
  group_by(date) %>% 
  summarise(sumSteps = sum(steps, na.rm = TRUE))
hist(total_steps_imputed$sumSteps, col = "blue", main="Total number of steps taken per day", xlab="Steps")
```

![](figure/unnamed-chunk-11-1.png)<!-- -->

Calculate and report the mean and median total number of steps taken per day

```r
meanSteps <- mean(total_steps_imputed$sumSteps)
meanSteps
```

```
## [1] 10766.19
```

```r
medianSteps <- median(total_steps_imputed$sumSteps)
medianSteps
```

```
## [1] 10766.19
```


## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
activity$date <- as.Date(strptime(activity$date, format="%Y-%m-%d"))
activity$datetype <- sapply(weekdays(activity$date), function(x) {
        if (x == "Saturday" | x =="Sunday") 
                {y <- "weekend"} else 
                {y <- "weekday"}
                y
        })
```

2. Make a panel plot containing a time series plot (i.e. type = "l"\color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
library(ggplot2)
average_by_datetype <- aggregate(steps~interval + datetype, data=activity, mean, na.rm=TRUE)
ggplot(average_by_datetype, aes(x = interval , y = steps, color = datetype)) +
       geom_line() +
       labs(title = "The average number of steps taken, averaged across all weekday days or weekend days", x = "Interval", y = "Steps") +
       facet_wrap(~datetype, ncol = 1, nrow=2)
```

![](figure/unnamed-chunk-15-1.png)<!-- -->
