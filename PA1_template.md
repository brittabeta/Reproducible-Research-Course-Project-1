---
title: "Reproducible Research Course Project 1"
author: "Britta"
date: "2023-12-09"
output: html_document
---



## Introduction

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

* *The data can be downloaded from the course web site*:

* Dataset: [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip)

* *The variables included in this dataset are*:

* steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)

* date: The date on which the measurement was taken in YYYY-MM-DD format

* interval: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

## Load dependencies


```r
library(tidyverse)
```

## Loading and preprocessing the data

**Note: Handling missing values is important!**  

In this project, I will do so as directed in the assignment instructions.


```r
# download data
path <- getwd()
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(url, file.path(path, "activityData.zip"))
# unzip
unzip(zipfile = "activityData.zip")
# read in data
ad <- read.csv("activity.csv")
# convert character to date
ad$date <- ymd(ad$date)
```

## What is mean total number of steps taken per day?

1. Calculate the total number of steps taken per day


```r
adsteps <- ad %>%
        group_by(date) %>%
        summarise(total.steps = sum(steps, na.rm = TRUE))
head(adsteps)
```

```
## # A tibble: 6 × 2
##   date       total.steps
##   <date>           <int>
## 1 2012-10-01           0
## 2 2012-10-02         126
## 3 2012-10-03       11352
## 4 2012-10-04       12116
## 5 2012-10-05       13294
## 6 2012-10-06       15420
```

2. Make a histogram of the total number of steps taken each day


```r
ggplot(data = adsteps, aes(total.steps)) +
        geom_histogram(fill = "purple3", color = "grey") +
        labs(x = "Steps per day", 
             y = "Frequency",
             title = "Distribution: Total Steps per Day",
             subtitle = "BEFORE handling of NA values")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![plot of chunk unnamed-chunk-4](https://github.com/brittabeta/Reproducible-Research-Course-Project-1/blob/master/instructions_fig/histogram_before.png)

3. Calculate and report the mean and median of the total number of steps taken per day


```r
stepstat <- adsteps %>%
        summarise(mean = mean(total.steps),
                  median = median(total.steps)) 
stepstat
```

```
## # A tibble: 1 × 2
##    mean median
##   <dbl>  <int>
## 1 9354.  10395
```

## What is the average daily activity pattern?

1. Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
# calculate average steps by interval across all days
avsteps <- ad %>%
        group_by(interval) %>%
        summarise(average.steps = mean(steps, na.rm = TRUE))
# plot a time series of average steps during the day
ggplot(data = avsteps, aes(interval, average.steps)) +
        geom_line(size = 1.2, color = "purple3") +
        labs(x = "5-minute Interval", 
             y = "Average Number of Steps",
             title = "Average Daily Activity Pattern")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png)

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
maxav <- subset(avsteps, average.steps == max(average.steps))
maxav
```

```
## # A tibble: 1 × 2
##   interval average.steps
##      <int>         <dbl>
## 1      835          206.
```

## Imputing missing values

There are a number of days/intervals where there are missing values (coded as NA). The presence of missing values may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset 

```r
sum(is.na(ad))
```

```
## [1] 2304
```

```r
# where do these missing values occur?
sum(is.na(ad$steps))
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. 

The strategy: impute NA values as the mean for that 5-minute interval


```r
# merge dataset with NAs with mean values based on interval
mrg <- merge(ad, avsteps, by = "interval")
# look at the before state
sum(is.na(mrg$steps))
```

```
## [1] 2304
```

```r
head(mrg)
```

```
##   interval steps       date average.steps
## 1        0    NA 2012-10-01      1.716981
## 2        0     0 2012-11-23      1.716981
## 3        0     0 2012-10-28      1.716981
## 4        0     0 2012-11-06      1.716981
## 5        0     0 2012-11-24      1.716981
## 6        0     0 2012-11-15      1.716981
```

```r
tail(mrg)
```

```
##       interval steps       date average.steps
## 17563     2355     0 2012-10-16      1.075472
## 17564     2355     0 2012-10-07      1.075472
## 17565     2355     0 2012-10-25      1.075472
## 17566     2355     0 2012-11-03      1.075472
## 17567     2355    NA 2012-10-08      1.075472
## 17568     2355    NA 2012-11-30      1.075472
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
# if the value in steps column is missing, replace with the mean
mrg$steps <- ifelse(is.na(mrg$steps), mrg$average.steps, mrg$steps)
# look at the after state
sum(is.na(mrg$steps))
```

```
## [1] 0
```

```r
head(mrg)
```

```
##   interval    steps       date average.steps
## 1        0 1.716981 2012-10-01      1.716981
## 2        0 0.000000 2012-11-23      1.716981
## 3        0 0.000000 2012-10-28      1.716981
## 4        0 0.000000 2012-11-06      1.716981
## 5        0 0.000000 2012-11-24      1.716981
## 6        0 0.000000 2012-11-15      1.716981
```

```r
tail(mrg)
```

```
##       interval    steps       date average.steps
## 17563     2355 0.000000 2012-10-16      1.075472
## 17564     2355 0.000000 2012-10-07      1.075472
## 17565     2355 0.000000 2012-10-25      1.075472
## 17566     2355 0.000000 2012-11-03      1.075472
## 17567     2355 1.075472 2012-10-08      1.075472
## 17568     2355 1.075472 2012-11-30      1.075472
```

4. Make a histogram of the total number of steps taken each day


```r
# calculate total steps by date 
mrgsteps <- mrg %>%
        group_by(date) %>%
        summarise(total.steps = sum(steps))
# plot the histogram
ggplot(data = mrgsteps, aes(total.steps)) +
        geom_histogram(fill = "violet", color = "purple") +
        labs(x = "Steps per day", 
             y = "Frequency",
             title = "Distribution: Total Steps per Day",
             subtitle = "AFTER handling of NA values")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png)

5. Calculate and report the mean and median total number of steps taken per day. 


```r
# calculate the post imputation mean and median of total steps
stepstat2 <- mrgsteps %>%
        summarise(mean = mean(total.steps),
                  median = median(total.steps)) 
stepstat2
```

```
## # A tibble: 1 × 2
##     mean median
##    <dbl>  <dbl>
## 1 10766. 10766.
```

6. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
# calculate the difference in mean and median (after impute - before)
stat <- stepstat2 - stepstat
stat
```

```
##       mean   median
## 1 1411.959 371.1887
```

The imputation of missing values has increased measures of central
tendency; both the mean and median have increased. The distribution
of total steps per day is now more normally distributed, as NA
values have been shifted towards the mean.

## Are there differences in activity patterns between weekdays and weekends?

1. Create a new variable with “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
# create a day of week variable
mrg$day <- wday(mrg$date)
# create a weekend vs weekday variable
mtof <- seq(1,5,1)
mrg$day <- ifelse(mrg$day %in% mtof, "Weekday", "Weekend")
```

2. Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 


```r
# calculate the average steps per interval and day
avmrg <- mrg %>%
        group_by(interval, day) %>%
        summarise(av.steps = mean(steps))
```

```
## `summarise()` has grouped output by 'interval'. You can override using the
## `.groups` argument.
```

```r
# plot the comparison of daily activity
ggplot(data = avmrg, aes(interval, av.steps, color = day)) +
        geom_line(size = 1.2) +
        facet_wrap(.~day) +
        labs(x = "5-minute Interval", 
             y = "Average Number of Steps",
             title = "Average Daily Activity Pattern")
```

![plot of chunk unnamed-chunk-15](figure/unnamed-chunk-15-1.png)

Thank you!
