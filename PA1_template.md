---
title: " **Reproducible Research: Peer graded assignment** "
author: " **Indira Srinivasan** "
date: "6/16/2020"
output: 
  html_document: 
    keep_md: yes
---

## HELLO!
*Welcome to my peer graded assignment - 1.*
*I hope you find my assignment in the best of spirits!*

## Let us begin! 


## Task 1

Loading and preprocessing the data
Load the data read.csv()
Process/transform the data (if necessary) into a format suitable for your analysis

So first I would like to download the activity monitoring dataset!

Downloading data


```r
knitr::opts_chunk$set(echo=TRUE) 
url<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(url,destfile="./amdataset.zip")
```

Then, I move to unzipping and reading the dataset using the read.csv function.

Reading in the data and viewing the unzipped data's first 5 rows


```r
unzipdf<-unzip("amdataset.zip")
activitydf<-read.csv(unzipdf)
head(activitydf)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

Tranforming the dataset: Preprocessing


```r
transformeddf<-na.omit(activitydf)
head(transformeddf)
```

```
##     steps       date interval
## 289     0 2012-10-02        0
## 290     0 2012-10-02        5
## 291     0 2012-10-02       10
## 292     0 2012-10-02       15
## 293     0 2012-10-02       20
## 294     0 2012-10-02       25
```

## Task 2

What is mean total number of steps taken per day? 

Creating a histogram of the total number of steps taken each day

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
## Grouping the data by date
groupedbydate<-group_by(transformeddf,date)
## Finding total number of steps taken each day
totsteps<-summarize_all(groupedbydate,sum)
## Plotting
library(ggplot2)
qplot(totsteps$steps,binwidth=1000,main="Total number of steps taken each day",xlab="Steps",ylab="Count")+theme_bw()
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

Reporting the mean of the total number of steps taken per day

```r
mean(totsteps$steps)
```

```
## [1] 10766.19
```
Reporting the median of the total number of steps taken per day

```r
median(totsteps$steps)
```

```
## [1] 10765
```

## Task 3

What is the average daily activity pattern?


A time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


## Plotting

```r
plot(steps~interval,data=meanperinterval,type="l",main="Average number of steps per interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps? 

```r
meanperinterval[which.max(meanperinterval$steps),1]
```

```
## # A tibble: 1 x 1
##   interval
##      <int>
## 1      835
```
## Task 4

Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NA/NAs)

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

Create a new dataset that is equal to the original dataset but with the missing data filled in.

Creating a clone of our activity monitoring dataset to impute:

```r
imputeddf<-activitydf
```

Checking the number of missing values in the activity dataset:

```r
sum(is.na(activitydf))
```

```
## [1] 2304
```

Imputing the missing values with the mean of each interval

```r
for(i in 1:nrow(imputeddf)){
  while(is.na(imputeddf$steps[i])){
    for(j in 1:nrow(meanperinterval)){
      if(meanperinterval$interval[j]==imputeddf$interval[i]){
        imputeddf$steps[i]<-meanperinterval$steps[j]
      }
    } 
  }
}
```

Confirming if there are any missing values left in the dataset

```r
sum(is.na(imputeddf))
```

```
## [1] 0
```

Do these values differ from the estimates from the first part of the assignment? 

```r
newgroupedbydate<-group_by(imputeddf,date)
newtotstepsperday<-summarize_all(newgroupedbydate,sum)
```

Answer a): Previous mean: 10766.19, New mean: 10766.19. Therefore, the mean stays the same.
This phenomenon occurs, because we have imputed the values with the mean of those respective intervals. Interval 0 gets the mean of the interval 0 and so on and so forth. Calculating the average value/ measure of central tendency is just strengthened by imputing the missing values with the mean (again, a measure of central tendency). 

New mean:

```r
mean(newtotstepsperday$steps)
```

```
## [1] 10766.19
```


Answer b): Previous median: 10765, New median: 10766.19. Therefore, the median changes. By replacing the missing values with the mean, we are increasing the count of the most occuring values and hence increasing the median.

New median: 

```r
median(newtotstepsperday$steps)
```

```
## [1] 10766.19
```


What is the impact of imputing missing data on the estimates of the total daily number of steps?

Before imputing: 

```r
sum(activitydf$steps,na.rm=TRUE)
```

```
## [1] 570608
```

After imputing:

```r
sum(imputeddf$steps)
```

```
## [1] 656737.5
```

The total number of steps have increased post imputing as can be seen from above.

Plotting a histogram to show increase in total steps

```r
qplot(newtotstepsperday$steps,main="Total steps each day (imputed)",xlab="Steps",ylab="Count",binwidth=1000)+theme_bw()
```

![](PA1_template_files/figure-html/unnamed-chunk-19-1.png)<!-- -->


## Task 5

Are there differences in activity patterns between weekdays and weekends?

Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

Make a panel plot containing a time series plot (type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 


```r
imputeddf$date<-as.Date(imputeddf$date)
imputeddf$Day<-weekdays(imputeddf$date)

## Adding 2 factor variables: weekend, weekday
for(i in 1:length(imputeddf$Day)){
  if(imputeddf$Day[i]=="Saturday"){
    imputeddf$myfactor[i]<-"weekend"
  }
  else if(imputeddf$Day[i]=="Sunday"){
    imputeddf$myfactor[i]<-"weekend"
  }

  else{
    imputeddf$myfactor[i]<-"weekday"
  }
}
## Factor variable creation 
imputeddf$myfactor<-factor(imputeddf$myfactor)
```

Aggregating the dataset and sorting it by interval and the factor variables

```r
aggimpudf<-aggregate(steps~interval+myfactor, imputeddf, mean)
```

Plotting the panel plot

```r
library(lattice)
xyplot(steps~interval|myfactor,data=aggimpudf,type="l",layout=c(1,2),main="Steps per interval on weekends and weekdays")
```

![](PA1_template_files/figure-html/unnamed-chunk-22-1.png)<!-- -->
