Reproducible Research Assignment 1
=================================================

##Loading and Preprocessing the Data.

First, load the data into an object "data".

```{r}
## Set working directory
setwd("D:/Xiao Xin/My Folder/Coursera/Data Science/05 Reproducible Research/Assignment 1")

## Read files
data <- read.csv("activity.csv")
```

Next, pre-process the data to the correct date and time format: 

  1. Convert date column into POSIX YYYY-MM-DD format.
  2. Convert interval column into POSIX HH:MM:SS format.

```{r}
## Convert character date to POSIX date
data$date <- as.POSIXct(strptime(data$date, "%Y-%m-%d"), tz="")

## Convert integer time to character and add leading zeros...
data$time <-sprintf("%04d", data$interval)

## Then convert to the data type
data$time <- as.POSIXct(data$time, "%H%M", tz="")
```

##Mean Total Number of Steps Per Day

Create a new data frame "totalsteps" which aggregates the total number of steps taken each day. 

```{r}
## Create new data frame for total steps per day
totalsteps <- aggregate(list(total_steps = data$steps),
                        by=list(date = data$date),
                        FUN=sum, 
                        na.rm=TRUE)
```

Plot a histogram to show the frequency of total steps.

```{r}
## Plot a histogram on the total number of steps taken per day
hist(totalsteps$total_steps,
     breaks=30,
     xlab="Total Steps",
     main="Total Steps Per Day",
     col="lightblue")
```

Calculate the mean and median of the total number of steps taken per day.

```{r}
## Calculate mean of total steps taken per day
mean(totalsteps$total_steps)

## Calculate median of total steps taken per day 
median(totalsteps$total_steps,na.rm=TRUE)
```

##Average Daily Activity Pattern

Create a new data frame that contains the average number of steps taken across all days for each time interval. 

```{r}
## Create new data frame for the average number of steps for each time interval
average_steps_by_time <- aggregate(list(average_steps = data$steps),
                                   by=list(time = data$time,
                                           interval = data$interval),
                                   FUN=mean,
                                   na.rm=TRUE)
```

Plot a line graph which shows the average number of steps at each time interval. 

```{r}
plot(average_steps ~ time,
     data=average_steps_by_time,
     xlab="Time Interval",
     ylab="Average Number of Steps",
     main="Average Number of Steps at Each Time Interval",
     type="l",
     col="blue",
     lwd=2)
```

Find out which time interval has the highest average number of steps. 

```{r}
average_steps_by_time[which.max(average_steps_by_time$average_steps),]
```

##Impute Missing Values

Count the number of rows with missing values (i.e. steps="NA").

```{r}
## Count the number of rows with "NA"
sum(is.na(data[,"steps"]))
```

Replace the missing values with the mean number of steps for that time interval, and put it in a new dataset "data_imputed".

```{r}
## Combine "data" with "average_steps_by_time" by the interval field
data_imputed <- merge(data, average_steps_by_time, by="interval")

## Replace NA values with average steps for the interval
## Create new dataset "data_imputed"
data_imputed <- within(data_imputed,
                        steps <- ifelse(is.na(data_imputed$steps),
                                           data_imputed$average_steps,
                                           data_imputed$steps))
```

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 

```{r}
## Calculate total number of steps per day
totalsteps2 <- aggregate(list(total_steps = data_imputed$steps),
                                         by=list(date = data_imputed$date),
                                         FUN=sum,
                                         na.rm=FALSE)

## Plot a histogram on the total number of steps taken per day
hist(totalsteps2$total_steps,
     breaks=30,
     xlab="Total Steps",
     main="Total Steps Per Day",
     col="lightblue")

## Calculate mean of total steps taken per day
mean(totalsteps2$total_steps)

## Calculate median of total steps taken per day 
median(totalsteps2$total_steps,na.rm=TRUE)
```



##Activity Pattern of Weekdays vs Weekends

Create a new factor variable in the dataset which indicates if a date is "weekday" or "weekend".

```{r}
## Add character column "day of week"
data_imputed$weekday  <- weekdays(data_imputed$date)

## Create a new factor column indicating "weekday", "weekend"
data_imputed$weekend_indicator <- as.factor(apply(data_imputed["weekday"], 1, function(x) {
  switch(x,
         "Sunday" = "weekend",
         "Saturday" = "weekend",
         "weekday")
}))
```

Create a plot to show the average number of steps taken at each time interval for "weekday" vs "weekend".

```{r}
## Create plot
average_steps_by_time2 <- aggregate(list(average_steps = data_imputed$steps),
                                    by=list(time = data_imputed$time.x,
                                            daytype = data_imputed$weekend_indicator),
                                    FUN=mean)
library(ggplot2)
qplot(x = time,
      y = average_steps,
      geom="path",
      data = average_steps_by_time2, 
      xlab="Time Interval",
      ylab="Average Steps",
      main="Activity Patterns\nWeekdays vs. Weekends",
      facets = daytype ~ .)
```


