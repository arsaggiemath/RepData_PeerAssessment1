# Reproducible Research: Peer Assessment 1
 
## Loading and preprocessing the data
Show any code that is needed to
 
1. Load the data (i.e. read.csv())
 

```r
activity <- read.csv("activity.csv")
```
 
2. Process/transform the data (if necessary) into a format suitable for your analysis
 

```r
library(lattice)
stepInterval <- aggregate(steps ~ interval, data = activity, mean, na.rm = TRUE)
```
 
## What is the mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.
 
1. Calculate the total number of steps taken per day
 

```r
stepCount <- aggregate(steps ~ date, data = activity, sum, na.rm = TRUE)
head(stepCount)
```

```
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```
 
2. Make a histogram of the total number of steps taken each day
 

```r
hist(stepCount$steps, main = "Total Steps per Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->
 
3. Calculate and report the mean and median of the total number of steps taken per day
 

```r
mean(stepCount$steps)
```

```
## [1] 10766.19
```

```r
median(stepCount$steps)
```

```
## [1] 10765
```
&nbsp;&nbsp;&nbsp;&nbsp;The mean total number of steps per day is **1.0766189\times 10^{4}**.
&nbsp;&nbsp;&nbsp;&nbsp;The median total number of steps per day is **10765**.
 
## What is the average daily activity pattern?
 
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
 

```r
plot(steps ~ interval, data = stepInterval, type = "l")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->
 
2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
 

```r
stepInterval[which.max(stepInterval$steps),]$interval
```

```
## [1] 835
```
&nbsp;&nbsp;&nbsp;&nbsp;The 5-minute interval with the maximum number of steps is the **835**-th interval.
 
## Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.
 
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
 

```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```
&nbsp;&nbsp;&nbsp;&nbsp;There are **2304** rows with NAs.
 
2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for the 5-minute interval, etc.

&nbsp;&nbsp;&nbsp;&nbsp;The strategy I am using is to replace missing values with the mean for that 5-minute interval.

```r
imputeSteps <- function(interval) {
    stepInterval[stepInterval$interval == interval,]$steps
}
```
 
3. Create a new dataset that is equal to the original dataset but with the missing data filled in
 
&nbsp;&nbsp;&nbsp;&nbsp;Fill in the missing data by calling to the function I created above.

```r
activityFull <- activity #Duplicate original dataset
i = 0 #Initialize counter
for (j in 1:nrow(activityFull)) {
    if(is.na(activityFull[j,]$steps)) {
        activityFull[j,]$steps <- imputeSteps(activityFull[j,]$interval)
        i = i + 1
    }
}
```
&nbsp;&nbsp;&nbsp;&nbsp;There were **2304** rows filled in.
 
4. Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
 

```r
stepTotal <- aggregate(steps ~ date, data = activityFull, sum)
hist(stepTotal$steps, main = "Total Steps per Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

```r
mean(stepTotal$steps)
```

```
## [1] 10766.19
```

```r
median(stepTotal$steps)
```

```
## [1] 10766.19
```
* &nbsp;&nbsp;&nbsp;&nbsp;The mean total number of steps per day is **1.0766189\times 10^{4}**.
* &nbsp;&nbsp;&nbsp;&nbsp;The median total number of steps per day is **1.0766189\times 10^{4}**.  
&nbsp;&nbsp;&nbsp;&nbsp;The mean value showed no change from the first part of this assignment while the median did alter slightly. Imputing the missing data on the estimates of the total daily number of steps brings the median up slightly (to match the mean) because of the method I selected to replace missing values. This could have been different had I used a different method for imputing missing values.
 
## Are there differences in activity patterns between weekdays and weekends?
For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.
 
1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
 

```r
activityFull$dayofweek = ifelse(as.POSIXlt(as.Date(activityFull$date))$wday%%6==0, "weekend", "weekday") #Assigns weekend to Saturday and Sunday
activityFull$day = factor(activityFull$day, levels = c("weekday", "weekend"))
```
 
2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend dats (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.
 

```r
stepDayOfWeek = aggregate(steps ~ interval + day, activityFull, mean)
xyplot(steps ~ interval | factor(day), data = stepDayOfWeek, aspect = 1/2, type = "l")
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->
