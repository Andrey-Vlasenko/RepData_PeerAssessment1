---
title: "Reproducible Research: Peer Assessment 1"
author: "Andrey Vlasenko"
date: "February 19, 2016"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
 
### 1. Setting Working directory


```r
rm(list=ls())
WorkingDir <- "C:/Users/vlasenko/YandexDisk/R_Programming/5_CourseProj1/RepData_PeerAssessment1/"
setwd(WorkingDir)
WorkingDir <- getwd()
print(paste("You can find data and results in ",WorkingDir))
```

```
## [1] "You can find data and results in  C:/Users/vlasenko/YandexDisk/R_Programming/5_CourseProj1/RepData_PeerAssessment1"
```

### 2. Setting file names


```r
Url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
F_name <- "repdata_data_activity.zip"
F_name_Data <- "activity.csv"
```

### 3. Downloading and unziping the data (if files aren't exist)


```r
if( !(F_name %in% dir()) ) { download.file(Url, destfile = F_name) }
if( !(F_name_Data %in% dir()) ) { unzip(F_name) }
```

### 4. Reading and  the data


```r
data <- read.csv(F_name_Data)
library(lubridate)
data$date <- ymd(data$date)
```

## What is mean total number of steps taken per day?

### 1. Evaluating sums of steps for each day (date), mean and median of sums


```r
# in our case we have 8 days with all empty values. 
# If we use na.rm then these empty day will stay with 0 sums.
Sum <- aggregate(data$steps, by = list(data$date), sum)
md <- median(Sum$x,na.rm=TRUE)
mn <- mean(Sum$x,na.rm=TRUE)
```

### 2. Plotting histogramm and reporting the mean and the median


```r
# histogramm only for not empty days.
hist(Sum$x, main="Histogram of the total number of steps taken each day", 
    xlab = "Steps per day", ylab = "Number of days", labels = TRUE)
text(20000, 25, paste("Median = ",md), col="blue")
text(20000, 20, paste("Mean = ",round(mn,2)),col="green")
abline(v=md, col="blue", lty = 19)
abline(v=mn, col="green", lty = 10)
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png)
### 3. Median and mean number of steps taken per day:


```r
# Median and Mean
print(paste("Median = ",md,"  Mean =", round(mn,5)))
```

```
## [1] "Median =  10765   Mean = 10766.18868"
```


## What is the average daily activity pattern?

### 1. Evaluating means of steps for each interval 


```r
Sum2 <- aggregate(data$steps, by = list(data$interval), mean, na.rm=TRUE)
md2 <- median(Sum2$x)
mn2 <- mean(Sum2$x)
```

### 2. Plotting graph and reporting the interval with max average number of spets


```r
MaxInt <- Sum2[Sum2$x == max(Sum2$x),1]
plot(Sum2$Group.1, Sum2$x, type="l",col="red",main="Daily activity pattern", 
    xlab = "interval", ylab = "steps")
text(1500, 200, paste("Max interval = ",MaxInt),col="black",cex=2)
text(2000, 170, paste("Median = ",round(md2,2)), col="blue")
text(2000, 150, paste("Mean = ",round(mn2,2)),col="green")
abline(h=md2, col="blue", lty = 19)
abline(h=mn2, col="green", lty = 10)
abline(v=MaxInt, col="black", lty = 1)
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png)


## Imputing missing values

### 1. Reporting missing values


```r
library(plyr)
MissedValues <- apply(is.na(data),2,sum)
MissedValues
```

```
##    steps     date interval 
##     2304        0        0
```

```r
barplot(MissedValues,main=paste("Missed values (total = ", sum(MissedValues),")"), 
    xlab = "variable", ylab = "Number")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png)

### 2. Filling missing values (and verify) using average daily activity pattern


```r
dat0 <- mutate(data, CorrectedSteps = ifelse(is.na(steps),
        Sum2[match(interval,Sum2$Group.1),2], steps) )
# Verify the filling of the missing values
missed <- as.integer(names(table(data[is.na(data$steps),3])))
missed_tbl <- cbind(missed,Sum2[match(missed, Sum2$Group.1),])
table(unique(dat0[is.na(dat0$steps),3:4]) == missed_tbl[,2:3])
```

```
## 
## TRUE 
##  576
```

### 3. Evaluating sums of steps for each day (date), mean and median of sums


```r
Sum3 <- aggregate(dat0$CorrectedSteps, by = list(dat0$date), sum)
md3 <- median(Sum3$x,na.rm=TRUE)
mn3 <- mean(Sum3$x,na.rm=TRUE)
```

### 4. Plotting histogramm and reporting the mean and the median


```r
hist(Sum3$x, main="Histogram of the total number of steps taken each day", 
    xlab = "Steps per day", ylab = "Number of days", labels = TRUE)
text(20000, 25, paste("Median = ",round(md3,2)), col="blue")
text(20000, 20, paste("Mean = ",round(mn3,2)),col="green")
abline(v=md3, col="blue", lty = 19)
abline(v=mn3, col="green", lty = 10)
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png)

```r
# Median and Mean
print(paste("Median = ",round(md3,5),"  Mean =", round(mn3,5)))
```

```
## [1] "Median =  10766.18868   Mean = 10766.18868"
```

## Are there differences in activity patterns between weekdays and weekends?

### 1. Creating a new factor variable in the dataset with two levels � �weekday� and �weekend�


```r
# Setting English for universality
language <- "English"
Sys.setlocale("LC_TIME", language)
```

```
## [1] "English_United States.1252"
```

```r
dat <- mutate(dat0, day = ifelse( (weekdays(dat0$date,abbreviate = TRUE)=="Sat")
| (weekdays(dat0$date,abbreviate = TRUE)=="Sun") , "weekend", "weekday"))
```

### 2. Evaluating means of steps for each interval


```r
Sum4 <- aggregate(dat$CorrectedSteps, by = list(dat$interval, dat$day), mean, na.rm=TRUE)
```

### 3. Making a panel plot containing a time series plots for weekdays and weekends


```r
library(ggplot2)
qplot(x=Group.1, y=x, data=Sum4, col=Group.2, facets=Group.2~., geom="line", 
      xlab="Interval",ylab="Number of steps", main = "")
```

![plot of chunk unnamed-chunk-16](figure/unnamed-chunk-16-1.png)


```r
## For creating reports use
#library(knitr)
#knit2html("PA1_template.Rmd", force_v1 = TRUE)
```
