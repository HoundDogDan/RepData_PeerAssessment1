---
title: "Reproducible Research - Peer Assessment 1"
author: "Danny Scott"
date: "Saturday, June 13, 2015"
output:
  html_document:
    keep_md: yes
---

#Reproducible Research - Peer Assesment 1

The data for this assignment can be downloaded from the course web site:

Dataset: Activity monitoring data [52K]

The variables included in this dataset are:

**steps:** Number of steps taking in a 5-minute interval (missing values are coded as NA)

**date:** The date on which the measurement was taken in YYYY-MM-DD format

**interval:** Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.





```r
#Bring in the libraries needed to process this script
library(ggplot2)
library(dplyr)
```



```r
#Read in the data file
all_activity <- read.csv(unz("activity.zip", filename="activity.csv"))


#format date variable
all_activity$date <- as.Date(all_activity$date)
```


##Question 1 - What is mean total number of steps taken per day?


```r
#daily sum (Total steps per day, ignor NAs)
sum_activity <- all_activity %>% filter(!is.na(steps)) %>% group_by(date) %>% summarize(sumsteps = sum(steps))
```


```r
#Histogram
hist(sum_activity$sumsteps, breaks=12, col="lightblue", xlab="total steps per day", main="Histogram of total steps per day" )
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

```r
#mean of total steps per day
mean(sum_activity$sumsteps)
```

```
## [1] 10766.19
```

```r
#median of total steps per day
median(sum_activity$sumsteps)
```

```
## [1] 10765
```

##Question 2 - What is the average daily activity pattern?


```r
#daily sum (Total steps per day, ignor NAs)
int_activity <- all_activity %>% filter(!is.na(steps)) %>% group_by(interval) %>% summarize(meansteps = mean(steps))
```

```r
#plot all that data
plot(int_activity$interval, int_activity$meansteps, type="l", main="Average steps by interval", xlab="5 min interval", ylab="avg steps")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png) 

```r
#the interval with the highest average no. of steps
max_avg_interval <- int_activity %>% filter(meansteps == max(meansteps))
```


##Question 3 - Imputing missing values


```r
#daily sum (Total steps per day, ignor NAs)
median_activity <- all_activity %>% filter(!is.na(steps)) %>% group_by(interval) %>% summarize(mediansteps = median(steps))
```

```r
#how many in-complete cases (truth table)
all_NA <- !complete.cases(all_activity)
```

```r
#the number of "NA" rows
sum(all_NA)
```

```
## [1] 2304
```


```r
#using the medians (median_activity), we'll fill in the missing data with the median for that interval

new_all_activity <- all_activity %>% filter(!is.na(steps))
new_all_na_activity <- all_activity %>% filter(is.na(steps))
```

```r
#combine the two, adding the "mediansteps" variable based on interval (index)
added <- merge(new_all_na_activity, median_activity, by="interval")
```

```r
#copy the values to the original steps variable
added$steps <- added$mediansteps
```

```r
#drop the column to prepare for the row bind
added$mediansteps <- NULL
new_df <- rbind(added, new_all_activity)
```


```r
#daily sum (Total steps per day, ignor NAs)
sum_new_df <- new_df %>% filter(!is.na(steps)) %>% group_by(date) %>% summarize(sumsteps = sum(steps))
```


```r
#Histogram
hist(sum_new_df$sumsteps, breaks=12, col="lightblue", xlab="total steps per day", main="Histogram of total steps per day" )
```

![plot of chunk unnamed-chunk-18](figure/unnamed-chunk-18-1.png) 


```r
#mean of total steps per day
mean(sum_new_df$sumsteps)
```

```
## [1] 9503.869
```


```r
#median of total steps per day
median(sum_new_df$sumsteps)
```

```
## [1] 10395
```



Do these values differ from the estimates from the first part of the assignment? 
Yes

What is the impact of imputing missing data on the estimates of the total daily number of steps?
changes the curve




##Question 4 - Are there differences in activity patterns between weekdays and weekends?


```r
#Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

new_df$weekend <- "weekday"
new_df$weekend[ which( (weekdays(new_df$date)=="Sunday") )] <- "weekend"
new_df$weekend[ which( (weekdays(new_df$date)=="Saturday") )] <- "weekend"

new_df$weekend <- as.factor(new_df$weekend)
```


```r
#daily sum (Total steps per day, ignor NAs)
new_int_activity <- new_df %>% group_by(weekend, interval) %>% summarize(meansteps = mean(steps))
```


```r
qplot(interval, meansteps, data=new_int_activity, facets = weekend ~ ., geom="line", main="Mean number of steps", ylab="mean steps")
```

![plot of chunk unnamed-chunk-23](figure/unnamed-chunk-23-1.png) 


