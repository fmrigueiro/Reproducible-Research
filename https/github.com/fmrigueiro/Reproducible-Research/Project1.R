setwd("~/Desktop/DATA")
if(!file.exists("getdata-projectfiles-UCI HAR Dataset.zip")) {
        temp <- tempfile()
        download.file("http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",temp)
        unzip(temp)
        unlink(temp)
}

data <- read.csv("activity.csv")

daily_steps <- aggregate(steps ~ date, data, sum)
hist(daily_steps$steps, main = paste("Total Steps Per Day"), col="red", xlab="# of Steps")
rmean <- mean(daily_steps$steps)
rmedian <- median(daily_steps$steps)


interval_steps <- aggregate(steps ~ interval, data, mean)

plot(interval_steps$interval,interval_steps$steps, type="l", xlab="Interval", ylab="Number of Steps",main="Average Number of Steps per Day by Interval", col = "blue")

incomplete <- sum(!complete.cases(data))
imputed_data <- transform(data, steps = ifelse(is.na(data$steps), interval_steps$steps[match(data$interval, interval_steps$interval)], data$steps))

imputed_data[as.character(imputed_data$date) == "2012-10-01", 1] <- 0

max_interval <- interval_steps[which.max(interval_steps$steps),1]

daily_steps_2 <- aggregate(steps ~ date, imputed_data, sum)
hist(daily_steps_2$steps, main = paste("Total Steps Each Day"), col="yellow", xlab="Number of Steps")

hist(daily_steps$steps, main = paste("Total Steps Each Day"), col="red", xlab="Number of Steps", add=T)
legend("topright", c("Imputed", "Non-imputed"), col=c("blue", "red"), lwd=10)

rmean.i <- mean(daily_steps_2$steps)
rmedian.i <- median(daily_steps_2$steps)

mean_diff <- rmean.i - rmean
med_diff <- rmedian.i - rmedian

total_diff <- sum(daily_steps_2$steps) - sum(daily_steps$steps)

weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", 
              "Friday")
imputed_data$dow = as.factor(ifelse(is.element(weekdays(as.Date(imputed_data$date)),weekdays), "Weekday", "Weekend"))

interval_steps_2 <- aggregate(steps ~ interval + dow, imputed_data, mean)

library(lattice)

xyplot(interval_steps_2$steps ~ interval_steps_2$interval|interval_steps_2$dow, main="Average Steps per Day by Interval",xlab="Interval", ylab="Steps",layout=c(1,2), type="l")
