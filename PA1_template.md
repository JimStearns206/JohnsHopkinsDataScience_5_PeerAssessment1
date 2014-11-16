# Reproducible Research: Peer Assessment 1
Jim Stearns  
16-Nov-2014  

(Assignment instructions are in *italics*)

## Loading and preprocessing the data

*Show any code that is needed to:*

*1. Load the data (i.e. read.csv()).*

*2. Process/transform the data (if necessary) into a format suitable for your analysis.*

Unzip a CSV file and load into a data frame. Leave - do not strip - observations with NA steps. Reasonable values will be interpolated below.


```r
activityMonitoringData_zipFileName <- "activity.zip"
stopifnot(file.exists(activityMonitoringData_zipFileName))
unzip(activityMonitoringData_zipFileName)
activityMonitoringData_fileName <- "activity.csv"
stepdf = read.csv(activityMonitoringData_fileName)

# Convert the date from a factor to a POSIX Date
stepdf$date <- as.Date(stepdf$date)
str(stepdf)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
# Provide a second dataframe with steps = NA omitted. But leave the full version - will be needed.
stepdf_nona <- stepdf[!is.na(stepdf$steps),]
```

## What is mean total number of steps taken per day?

*For this part of the assignment, you can ignore the missing values in the dataset.*

*1. Make a histogram of the total number of steps taken each day*

Aggregrate all the step entries for each date, using the dataset excluding observations with NA steps:


```r
stepsbydate <- aggregate(stepdf_nona$steps, by=list(stepdf_nona$date), FUN=sum)
names(stepsbydate) <- c("date","steps")
stepsbydate$date <- as.Date(stepsbydate$date)
```

And the histogram:


```r
plot(stepsbydate$date, stepsbydate$steps, type="h")
```

![](PA1_template_files/figure-html/plot-date-vs-stepsByDate-noNA-1.png) 

*2. Calculate and report the mean and median total number of steps taken per day*

Some days have no entries: all of their entries have NA in the steps column. E.g. 2012-11-01.
Decided: exclude dates without at least one non-NA observation of steps. Can be zero, 
but all NAs means that date is excluded from calculation of mean and median.


```r
meanSteps <- mean(stepsbydate$steps)
medianSteps <- median(stepsbydate$steps)
```

Steps taken per day: mean=10766.2, median=10765.0

## What is the average daily activity pattern?

*1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)*

Using the dataset with NAs removed, aggregate by interval across all the observations:

```r
stepsbyinterval <- aggregate(stepdf_nona$steps, by=list(stepdf_nona$interval), FUN=mean)
# Rename the columns
names(stepsbyinterval) <- c("intervalMinuteStart", "steps")
# Add an sequence number for each interval, 0 .. 287
minutesPerInterval <- 5
stepsbyinterval$intervalIndex <- stepsbyinterval$intervalMinuteStart / minutesPerInterval
```

```r
## TODO: Improve x-axis labeling.
plot(stepsbyinterval$intervalIndex, stepsbyinterval$steps, type="l", 
     main="Average Steps in 5-minute Intervals, Oct/Nov-2012")
```

![](PA1_template_files/figure-html/time-series-plot-interval-vs-average-steps-1.png) 

*2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?*

```r
maxStepInterval <- stepsbyinterval[stepsbyinterval$steps == max(stepsbyinterval$steps),]
militaryMinuteInterval <- maxStepInterval$intervalMinuteStart
intervalStartHHMM <- paste(trunc(militaryMinuteInterval/100), militaryMinuteInterval %% 100, sep=":")
```

The 5-minute interval with the largest average number of steps starts at 8:35 (in 24-hour military time).
It contains 206 steps.

## Imputing missing values

*Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.*

*1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)*


```r
nIncompleteCases <- sum(!complete.cases(stepdf))
nStepsAreNA <- sum(is.na(stepdf$steps))
naStepsVisavisIncompleteCases <- 'some incomplete cases have non-NA steps'
if (nStepsAreNA == nIncompleteCases)
{
    naStepsVisavisIncompleteCases <- 'all incomplete cases have NA steps'
}
```
The number of step-interval observations (rows) with missing values is 2304.
The number of these incomplete rows with missing (NA) steps value is 2304 
(all incomplete cases have NA steps).

*2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.*

The algorithm chosen for imputing a value to a NA steps: use the truncated (integer) value of the 
average number of steps for that time-of-day interval computed over two months.

*3. Create a new dataset that is equal to the original dataset but with the missing data filled in.*


```r
stepdf_imputed <- stepdf
for (row in 1:nrow(stepdf_imputed))
{
    if (is.na(stepdf_imputed$steps[row]))
    {
        thisRowInterval <- stepdf_imputed$interval[row]
        imputedStepValue <- round(stepsbyinterval$steps[thisRowInterval == stepsbyinterval$intervalMinuteStart])
        stepdf_imputed$steps[row] <- imputedStepValue
    }
}
```
Quick cross-checks:

* Number of entries in stepdf_imputed with NA steps: 0 (should be 0)

* Number of rows with newly non-NA step values should equal number of rows in stepdf with NA steps:

```r
nNaSteps <- sum(is.na(stepdf$steps))
nNewlyNonNaSteps <- sum(is.na(stepdf$steps) & !is.na(stepdf_imputed$steps))
```
TRUE (2304 == 2304)

*4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.*


```r
stepsbydate_imputed <- aggregate(stepdf_imputed$steps, by=list(stepdf_imputed$date), FUN=sum)
names(stepsbydate_imputed) <- c("date","steps")
stepsbydate_imputed$date <- as.Date(stepsbydate_imputed$date)
```


```r
plot(stepsbydate_imputed$date, stepsbydate_imputed$steps, type="h")
```

![](PA1_template_files/figure-html/plot-date-vs-stepsByDate-NAimputed-1.png) 


```r
meanSteps_imputed <- mean(stepsbydate_imputed$steps)
medianSteps_imputed <- median(stepsbydate_imputed$steps)
```
Steps, including imputed, taken per day: mean=10765.6, median=10762.0

*Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?*

Imputing values to NA steps has little or no impact on the mean or median steps taken.

## Are there differences in activity patterns between weekdays and weekends?

*For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.*

*1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.*


```r
stepdf_imputed$daytype <- as.factor(weekdays(as.Date(stepdf_imputed$date)) %in% c('Saturday', 'Sunday'))
levels(stepdf_imputed$daytype)[levels(stepdf_imputed$daytype) == TRUE] <- "weekend"
levels(stepdf_imputed$daytype)[levels(stepdf_imputed$daytype) == FALSE] <- "weekday"
str(stepdf_imputed$daytype)
```

```
##  Factor w/ 2 levels "weekday","weekend": 1 1 1 1 1 1 1 1 1 1 ...
```

*2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.*

(NOT DONE)
