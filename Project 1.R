setwd("~/Reproducible research")
activity<-read.csv("activity.csv")

dim(activity)
names(activity)
head(activity)
str(activity)

sum(is.na(activity$steps))

library(lubridate)
activity$date<-ymd(activity$date)
length(unique(activity$date))

#What is mean total number of steps taken per day?
#Calculate the total number of steps taken per day
library(ggplot2)
library(plyr)
totalsteps <- aggregate(activity$steps ~ activity$date, FUN=sum, )
colnames(totalsteps)<- c("Date", "Steps")

#If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day
hist(totalsteps$Steps)

#Calculate and report the mean and median of the total number of steps taken per day
mean(totalsteps$Steps)
median(totalsteps$Steps)

#What is the average daily activity pattern?
#Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
stepsPerInterval<-aggregate(steps~interval, data=activity, mean, na.rm=TRUE)
plot(steps~interval, data=stepsPerInterval, type="l")

#Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
stepsPerInterval[which.max(stepsPerInterval$steps),]$interval

#Imputing missing values
#Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with ????????s)
sum(is.na(activity$steps))

#Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
m<-mean(stepsPerInterval$steps)

#Create a new dataset that is equal to the original dataset but with the missing data filled in.
missingIndex<-is.na(activity[,1])
m<-mean(stepsPerInterval$steps)
new<-activity
new[missingIndex,1]<-m
head(new)

#Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
totalStepsByDay1<-aggregate(steps~date, new, FUN=sum)
hist(totalStepsByDay1$steps, xlab="Class of Total Number of Steps per day", ylab="Number of Days", main="Number of Steps taken each day after missing values are imputed")
mean(totalStepsByDay1$steps)
median(totalStepsByDay1$steps)

#Are there differences in activity patterns between weekdays and weekends?
#For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.
new$date<-as.Date(new$date)
library(dplyr)

activity2<-new%>%
  mutate(dayType= ifelse(weekdays(new$date)=="Saturday" | weekdays(new$date)=="Sunday", "Weekend", "Weekday"))
head(activity2)

#Make a panel plot containing a time series plot (i.e. ???????????????? = “????”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.
averageStepByDayTypeAndInterval<-activity2 %>%
  group_by(dayType, interval) %>%
  summarize(averageStepByDay=sum(steps))

head(averageStepByDayTypeAndInterval)

library(lattice)
with(averageStepByDayTypeAndInterval, 
     xyplot(averageStepByDay ~ interval | dayType, 
            type = "l",      
            main = "Total Number of Steps within Intervals by dayType",
            xlab = "Daily Intervals",
            ylab = "Average Number of Steps"))




