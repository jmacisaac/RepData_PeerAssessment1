---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

## Loading and preprocessing the data

```r
activity_raw<- read.csv('activity.csv')
activity <- na.omit(activity_raw)
```
## What is mean total number of steps taken per day?

```r
steps_day <- aggregate(steps ~ date, activity, sum)

hist(steps_day$steps,
	   main="Total number of steps per day", 
     xlab="Steps/Day",
	   ylab="Qty",
	   col="blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
summary(steps_day)
```

```
##          date        steps      
##  2012-10-02: 1   Min.   :   41  
##  2012-10-03: 1   1st Qu.: 8841  
##  2012-10-04: 1   Median :10765  
##  2012-10-05: 1   Mean   :10766  
##  2012-10-06: 1   3rd Qu.:13294  
##  2012-10-07: 1   Max.   :21194  
##  (Other)   :47
```
## What is the average daily activity pattern?

```r
avg_interval <- aggregate(steps ~ interval, activity, mean)

plot(avg_interval$interval, 
	   avg_interval$steps,
	 type='l', 
	 main="Average Daily Activity Pattern", 
     xlab="Interval",
     ylab="Average Steps Taken",
	 col="blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
avg_interval[row_max <- which.max(avg_interval$steps), ]
```

```
##     interval    steps
## 104      835 206.1698
```
## Imputing missing values

```r
sum(is.na(activity_raw))
```

```
## [1] 2304
```

```r
activity_new <- (activity_raw)
activity_new$steps[is.na(activity_new$steps)] <- mean(na.omit(activity_raw$steps))

steps_day_new <- aggregate(steps ~ date, activity_new, sum)

hist(steps_day_new$steps,
	 main="Total number of steps per day", 
     xlab="Steps/Day",
	 ylab="Qty",
	 col="blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
round(mean(steps_day_new$steps))
```

```
## [1] 10766
```

```r
round(median(steps_day_new$steps))
```

```
## [1] 10766
```
## Are there differences in activity patterns between weekdays and weekends?

```r
activity_raw$date <- as.Date(activity_raw$date)
activity_raw$day <- weekdays(activity_raw$date)
activity_raw$week_wkd <- "weekday"

activity_raw$week_wkd[activity_raw$day %in% c("Saturday", "Sunday")] <- "weekend"

step5 <- aggregate(steps ~ interval + week_wkd, activity_raw, mean)

library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.4.3
```

```r
qplot(interval, 
      steps, 
      data = step5, 
      type = "l", 
      geom="line",
      xlab = "Interval", 
      ylab = "# of steps", 
      main = "activity patterns between weekdays and weekends",
      facets =week_wkd ~ .)
```

```
## Warning: Ignoring unknown parameters: type
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
