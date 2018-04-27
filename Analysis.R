
library(ggplot2)

#Extracting data
#I will be commenting some part when I publish as the code snippet has already been run

setwd("D:/SANDY/Enter The Dragon/Data Science")
#if(!file.exists("./RERESCH")){dir.create("./RERESCH")}
#fileUrl <- "http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
#download.file(fileUrl,destfile="./RERESCH/RERESCH.zip")
# Unzip dataSet to /data directory
#unzip(zipfile="D:/SANDY/Enter The Dragon/Data Science/RERESCH/RERESCH.zip",exdir="./RERESCH")

activity_data <- read.csv("./RERESCH/activity.csv")



 print(sum(is.na(activity_data)))

#What is mean total number of steps taken per day?
# -- Calculate the total number of steps taken per day-- 


activity_steps_day <- aggregate(steps ~ date, data = activity_data, FUN = sum, na.rm = TRUE)
hist(activity_steps_day$steps, xlab = "Steps per Day", main = "Total number of steps taken per day", col = "wheat")


#What is average number of steps taken per day?
# -- Calculate the average total number of steps taken per day with a time plot


activity_steps_mean <- aggregate(steps ~ interval, data = activity_data, FUN = mean, na.rm = TRUE)
#Plot the graph
plot(activity_steps_mean$interval, activity_steps_mean$steps, type = "l", col = "red", xlab = "Intervals", ylab = "Time Series for Total steps per interval", main = "Number of steps per interval (averaged)")



#what is the highest steps value? (maximum of steps on one given interval)
max_steps <-max(activity_steps_mean$steps)
#for which interval are the numbers of steps per interval at the highest?
max_interval <- activity_steps_mean$interval[which(activity_steps_mean$steps == max_steps)]
max_steps <- round(max_steps, digits = 0)



mean_steps <- mean(activity_steps_day$steps)
median_steps <- median(activity_steps_day$steps)

#we set a normal number format to display the results

mean_steps <- format(mean_steps,digits=1)
median_steps <- format(median_steps,digits=1)

print(mean_steps)

print(median_steps)


# Adding Missing values

act_temp <- activity_data
print(sum(is.na(act_temp)))

#Missing values
nas <- is.na(act_temp$steps)

avg_interval <- tapply(act_temp$steps, act_temp$interval, mean, na.rm=TRUE, simplify=TRUE)

act_temp$steps[nas] <- avg_interval[as.character(act_temp$interval[nas])]

activity_steps_day_nas <- aggregate(steps ~ date, data = act_temp, FUN = sum, na.rm = TRUE)

head(activity_steps_day_nas)

mean_steps_nas <- mean(activity_steps_day_nas$steps)
median_steps_nas <- median(activity_steps_day_nas$steps)

df.activity_steps_day_nas <- as.data.frame(activity_steps_day_nas)

#Make a histogram of the total number of steps taken each day-- 

ggplot(df.activity_steps_day_nas, aes(x= steps))+
    geom_histogram(fill = "steelblue", binwidth = 1000)+
    labs(title = "Histogram of Total Steps per day - Modified Data", x = "Total Steps per day", y = "Frequency")



print(mean_steps_nas)

print(median_steps_nas)


# Observations on Mean and Median

#Are there differences in activity patterns between weekdays and weekends?

##Create a new factor variable in the dataset with two levels - "Weekday" and "Weekend" indicating whether a given date is a weekday or weekend day.

df.act_temp <- as.data.frame(act_temp)
df.act_temp$day <- weekdays(as.Date(act_temp$date))
df.act_temp$weekend = chron::is.weekend(df.act_temp$date)
df.act_temp$weekend[df.act_temp$weekend == "FALSE"] <- "Weekday"
df.act_temp$weekend[df.act_temp$weekend == "TRUE"] <- "Weekend"
# Doing it this way as mutate does not work on 3.3.1

DayTypeAvgSteps <- aggregate(steps ~ interval + weekend, data = df.act_temp, FUN = mean)
head(DayTypeAvgSteps)

df.DayTypeAvgSteps <- as.data.frame(DayTypeAvgSteps)

names(DayTypeAvgSteps) <- c("interval", "day_type", "mean_steps")

#Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

library(lattice)

xyplot(mean_steps~interval|factor(day_type),
        data = DayTypeAvgSteps,
       type='l',layout=c(1,2),
       xlab='5 min Interval',ylab='Number of Steps')

