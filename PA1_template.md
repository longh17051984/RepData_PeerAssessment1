##### 1. Read data

    if(!file.exists('activity.csv')){
        unzip('activity.zip')
    }
    data <- read.csv('activity.csv')

##### 2. Process/transform the data into a format suitable for your analysis

    data$date <- as.Date(data$date, format = "%Y-%m-%d")

What is mean total number of steps taken per day?
-------------------------------------------------

##### 1. Calculate the total number of steps taken per day

    stepsPerDay <- tapply(data$steps, data$date, sum, na.rm=TRUE)

##### 2. Make a histogram of the total number of steps taken each day

    qplot(stepsPerDay,xlab='Total steps/day', ylab='Frequency with binning 500', binwidth=500,fill=I("grey"))

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-5-1.png)

##### 3. Calculate and report the mean and median of the total number of steps taken per day

    meanSTPerDay <- mean(stepsPerDay)
    medianSTPerDay <- median(stepsPerDay)

-   Mean: 9354.2295082
-   Median: 10395

------------------------------------------------------------------------

What is the average daily activity pattern?
-------------------------------------------

    avgStPerBlockTime <- aggregate(x=list(meanSteps=data$steps), by=list(interval=data$interval), FUN=mean, na.rm=TRUE)

##### 1.Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

    ggplot(data=avgStPerBlockTime, aes(x=interval, y=meanSteps)) +
        geom_line(color="blue") +
        xlab("5-minute interval") +
        ylab("Average number of steps taken") 

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-8-1.png)

##### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

    maxStep <- which.max(avgStPerBlockTime$meanSteps)
    timeHaveMaxStep <-  gsub("([0-9]{1,2})([0-9]{2})", "\\1:\\2", avgStPerBlockTime[maxStep,'interval'])

-   Max Steps at: 8:35

------------------------------------------------------------------------

Imputing missing values
-----------------------

##### 1. Calculate and report the total number of missing values in the dataset

    numMSValue <- length(which(is.na(data$steps)))

-   Number of missing values: 2304

##### 2. Devise a strategy for filling in all of the missing values in the dataset.

-   The mean will be applied for that 5-minute interval to filling
    missing value\*

##### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

    dtImputed <- data
    dtImputed$steps <- impute(data$steps, fun=mean)

##### 4. Make a histogram of the total number of steps taken each day

    stPerDayIP <- tapply(dtImputed$steps, dtImputed$date, sum)
    qplot(stPerDayIP, xlab='Total steps per day with data Imputed', ylab='Frequency with binning 500', binwidth=500,fill=I("grey"))

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-12-1.png)

##### ... and Calculate and report the mean and median total number of steps taken per day.

    meanSTPerDayImputed <- mean(stPerDayIP)
    medianSTPerDayImputed <- median(stPerDayIP)

-   Mean (Imputed): 1.076618910^{4}
-   Median (Imputed): 1.076618910^{4}

------------------------------------------------------------------------

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

##### 1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

    dtImputed$dateCat <-  ifelse(as.POSIXlt(dtImputed$date)$wday %in% c(0,6), 'weekend', 'weekday')

##### 2. Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)

    avgImputed <- aggregate(steps ~ interval + dateCat, data=dtImputed, mean)
    ggplot(avgImputed, aes(interval, steps)) + 
        geom_line(color="blue") + 
        facet_grid(dateCat ~ .) +
        xlab("5-minute inv") + 
        ylab("AVG num of steps")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-15-1.png)
