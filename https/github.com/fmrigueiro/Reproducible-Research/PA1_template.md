---
title: "Analyzing Tracker Data"
output:
  html_document: default
  pdf_document: default
---
###About
This was the first project for the **Reproducible Research** course in Coursera's Data Science specialization track. The purpose of the project was to answer a series of questions using data collected from a [FitBit](http://en.wikipedia.org/wiki/Fitbit).


##Synopsis
The purpose of this project was to practice:

* loading and preprocessing data
* imputing missing values
* interpreting data to answer research questions

## Data
The data for this assignment was downloaded from the course web
site:

* Dataset: [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) [52K]

The variables included in this dataset are:

* **steps**: Number of steps taking in a 5-minute interval (missing
    values are coded as `NA`)

* **date**: The date on which the measurement was taken in YYYY-MM-DD
    format

* **interval**: Identifier for the 5-minute interval in which
    measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

## Loading and preprocessing the data

Download, unzip and load data into data frame `data`. 
```{r}
if(!file.exists("getdata-projectfiles-UCI HAR Dataset.zip")) {
        temp <- tempfile()
        download.file("http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",temp)
        unzip(temp)
        unlink(temp)
}

data <- read.csv("activity.csv")
```


## What is mean total number of steps taken per day?
Sum steps by day, create Histogram, and calculate mean and median.
```{r} 
daily_steps <- aggregate(steps ~ date, data, sum)
hist(daily_steps$steps, main = paste("Total Steps Per Day"), col="red", xlab="# of Steps")
rmean <- mean(daily_steps$steps)
rmedian <- median(daily_steps$steps)
```

The `mean` is `r rmean` and the `median` is `r rmedian`.

## What is the average daily activity pattern?

* Calculate average steps for each interval for all days. 
* Plot the Average Number Steps per Day by Interval. 
* Find interval with most average steps. 
```{r}
interval_steps <- aggregate(steps ~ interval, data, mean)

plot(interval_steps$interval,interval_steps$steps, type="l", xlab="Interval", ylab="Number of Steps",main="Average Number of Steps per Day by Interval", col = "blue")

max_interval <- interval_steps[which.max(interval_steps$steps),1]
```

The 5-minute interval, on average across all the days in the data set, containing the maximum number of steps is `r max_interval`.

## Impute missing values. Compare imputed to non-imputed data.
Missing data needed to be imputed. Only a simple imputation approach was required for this assignment. 
Missing values were imputed by inserting the average for each interval. Thus, if interval 10 was missing on 10-02-2012, the average for that interval for all days (0.1320755), replaced the NA. 
```{r}
iincomplete <- sum(!complete.cases(data))
imputed_data <- transform(data, steps = ifelse(is.na(data$steps), interval_steps$steps[match(data$interval, interval_steps$interval)], data$steps))
```

Zeroes were imputed for 10-01-2012 because it was the first day and would have been over 9,000 steps higher than the following day, which had only 126 steps. NAs then were assumed to be zeros to fit the rising trend of the data. 
```{r}
imputed_data[as.character(imputed_data$date) == "2012-10-01", 1] <- 0
```

Recount total steps by day and create Histogram. 
```{r}
daily_steps_2 <- aggregate(steps ~ date, imputed_data, sum)
hist(daily_steps_2$steps, main = paste("Total Steps Each Day"), col="yellow", xlab="Number of Steps")


hist(daily_steps$steps, main = paste("Total Steps Each Day"), col="red", xlab="Number of Steps", add=T)
legend("topright", c("Imputed", "Non-imputed"), col=c("yellow", "red"), lwd=10)
```

Calculate new mean and median for imputed data. 
```{r}
rmean.i <- mean(daily_steps_2$steps)
rmedian.i <- median(daily_steps_2$steps)
```

Calculate difference between imputed and non-imputed data.
```{r}
mean_diff <- rmean.i - rmean
med_diff <- rmedian.i - rmedian
```

Calculate total difference.
```{r}
total_diff <- sum(daily_steps_2$steps) - sum(daily_steps$steps)
```
* The imputed data mean is `r rmean.i`
* The imputed data median is `r rmedian.i`
* The difference between the non-imputed mean and imputed mean is `r mean_diff`
* The difference between the non-imputed mean and imputed mean is `r med_diff`
* The difference between total number of steps between imputed and non-imputed data is `r total_diff`. Thus, there were `r total_diff` more steps in the imputed data.


## Are there differences in activity patterns between weekdays and weekends?
Created a plot to compare and contrast number of steps between the week and weekend. There is a higher peak earlier on weekdays, and more overall activity on weekends.  
``` {r}
weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", 
              "Friday")
imputed_data$dow = as.factor(ifelse(is.element(weekdays(as.Date(imputed_data$date)),weekdays), "Weekday", "Weekend"))

interval_steps_2 <- aggregate(steps ~ interval + dow, imputed_data, mean)

library(lattice)

xyplot(interval_steps_2$steps ~ interval_steps_2$interval|interval_steps_2$dow, main="Average Steps per Day by Interval",xlab="Interval", ylab="Steps",layout=c(1,2), type="l")

```

