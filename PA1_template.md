---
title: "Reproducible Research Peer Assignment 1"
author: "Harish Krishnan"
date: "Saturday, May 16, 2015"
output: html_document
---

### Universal

```r
echo = TRUE
```

### Loading and processing the data

```r
unzip("activity.zip") # Place the activity.zip folder on your root folder. In my case, documents folder.
data <- read.csv("activity.csv", colClasses = c("integer", "Date", "factor"))
data$month <- as.numeric(format(data$date, "%m"))
complete <- na.omit(data) # Store all the non NA data seperately
rownames(complete) <- 1:nrow(complete)
head(complete)
```

```
##   steps       date interval month
## 1     0 2012-10-02        0    10
## 2     0 2012-10-02        5    10
## 3     0 2012-10-02       10    10
## 4     0 2012-10-02       15    10
## 5     0 2012-10-02       20    10
## 6     0 2012-10-02       25    10
```

```r
dim(complete)
```

```
## [1] 15264     4
```

```r
library(ggplot2)
```


### What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.

* Make a histogram of the total number of steps taken each day

```r
ggplot(complete, aes(date, steps)) + geom_bar(stat = "identity", colour = "light green", fill = "light green", width = 1) + facet_grid(. ~ month, scales = "free") + labs(title = "Histogram of Total number of steps Taken Each Day", x = "Date", y = "Total number of steps") + theme_bw()
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

* Calculate and report the mean and median total number of steps taken per day

Mean total number of steps taken per day:

```r
allSteps <- aggregate(complete$steps, list(Date = complete$date), FUN = "sum")$x
mean(allSteps)
```

```
## [1] 10766.19
```
Median total number of steps taken per day:

```r
median(allSteps)
```

```
## [1] 10765
```

### What is the average daily activity pattern?
* Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
avgSteps <- aggregate(complete$steps, list(interval = as.numeric(as.character(complete$interval))), FUN = "mean")
names(avgSteps)[2] <- "meanOfSteps"

ggplot(avgSteps, aes(interval, meanOfSteps)) + geom_line(color = "light green", size = 1) + labs(title = "Time Series Plot of the 5-minute Interval", x = "5-minute intervals", y = "Average number of steps Taken") + theme_classic()
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 

* Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
avgSteps[avgSteps$meanOfSteps == max(avgSteps$meanOfSteps), ]
```

```
##     interval meanOfSteps
## 104      835    206.1698
```

### Imputing missing values
* The total number of rows with NAs:


```r
sum(is.na(data))
```

```
## [1] 2304
```

* Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

I have decided to use the mean for that 5-minute interval to fill each NA value in the steps column.

* Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
completeData <- data 
for (i in 1:nrow(completeData)) {
    if (is.na(completeData$steps[i])) {
        completeData$steps[i] <- avgSteps[which(completeData$interval[i] == avgSteps$interval), ]$meanOfSteps
    }
}

head(completeData)
```

```
##       steps       date interval month
## 1 1.7169811 2012-10-01        0    10
## 2 0.3396226 2012-10-01        5    10
## 3 0.1320755 2012-10-01       10    10
## 4 0.1509434 2012-10-01       15    10
## 5 0.0754717 2012-10-01       20    10
## 6 2.0943396 2012-10-01       25    10
```

```r
sum(is.na(completeData))
```

```
## [1] 0
```

* Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 


```r
ggplot(completeData, aes(date, steps)) + geom_bar(stat = "identity",
                                             colour = "light green",
                                             fill = "light green",
                                             width = 1) + facet_grid(. ~ month, scales = "free") + labs(title = "Histogram of Total number of steps Taken Each Day (complete)", x = "Date", y = "Total number of steps")+theme_bw()
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 

* Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

Mean total number of steps taken per day:

```r
newallSteps <- aggregate(completeData$steps, 
                           list(Date = completeData$date), 
                           FUN = "sum")$x
newMean <- mean(newallSteps)
newMean
```

```
## [1] 10766.19
```
Median total number of steps taken per day:

```r
newMedian <- median(newallSteps)
newMedian
```

```
## [1] 10766.19
```
Compare them with the two before imputing missing data:

```r
oldMean <- mean(allSteps)
oldMedian <- median(allSteps)
newMean - oldMean
```

```
## [1] 0
```

```r
newMedian - oldMedian
```

```
## [1] 1.188679
```
Inference: new mean same as that of the old mean, new median greater than the old median. 

### Are there differences in activity patterns between weekdays and weekends?

* Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
head(completeData)
```

```
##       steps       date interval month
## 1 1.7169811 2012-10-01        0    10
## 2 0.3396226 2012-10-01        5    10
## 3 0.1320755 2012-10-01       10    10
## 4 0.1509434 2012-10-01       15    10
## 5 0.0754717 2012-10-01       20    10
## 6 2.0943396 2012-10-01       25    10
```

```r
completeData$weekdays <- factor(format(completeData$date, "%A"))
levels(completeData$weekdays)
```

```
## [1] "Friday"    "Monday"    "Saturday"  "Sunday"    "Thursday"  "Tuesday"  
## [7] "Wednesday"
```

```r
levels(completeData$weekdays) <- list(weekday = c("Monday", "Tuesday","Wednesday", 
                                                  "Thursday", "Friday"),
                                 weekend = c("Saturday", "Sunday"))
levels(completeData$weekdays)
```

```
## [1] "weekday" "weekend"
```

```r
table(completeData$weekdays)
```

```
## 
## weekday weekend 
##   12960    4608
```

* Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
avgSteps <- aggregate(completeData$steps, 
                      list(interval = as.numeric(as.character(completeData$interval)), 
                           weekdays = completeData$weekdays),
                      FUN = "mean")
names(avgSteps)[3] <- "meanOfSteps"

library(lattice)
xyplot(avgSteps$meanOfSteps ~ avgSteps$interval | avgSteps$weekdays, 
       layout = c(1, 2), type = "l", xlab = "Interval", ylab = "Number of steps")
```

![plot of chunk unnamed-chunk-15](figure/unnamed-chunk-15-1.png) 
