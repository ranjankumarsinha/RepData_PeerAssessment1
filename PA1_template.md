---
title: "PA1_template"
author: "Ranjan Kumar"
date: "13 November 2015"
output: html_document
---

# R Markdown document for Peer Assessment 1.

## Summary
This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

### Data Processing
1. Download data and Unzip it


```r
  fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
  #download.file(fileUrl, destfile = "activity_monitoring.zip", method = "curl")
  #unzip("activity_monitoring.zip")
```

2. Load the data into R for Analysis

```r
  activity <- read.csv("activity.csv",header = TRUE)
  activity$date <- as.Date(as.character(activity$date),"%Y-%m-%d")
```

3. Convert *data frame* into *data frame tbl* and Convert **Date** variable as Date datatype

```r
  library(dplyr)
  activity <- tbl_df(activity)
```

4. Exploration of dataset.

```r
  summary(activity)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0  
##  NA's   :2304
```

5. Filter *missing value* **NA** from dataset and then Calculate the total number of steps taken per day

```r
  steps_by_day <- activity %>% 
                        filter(!is.na(steps)) %>%
                        group_by(date) %>% 
                        summarize(tot_steps = sum(steps))
  steps_by_day
```

```
## Source: local data frame [53 x 2]
## 
##          date tot_steps
##        (date)     (int)
## 1  2012-10-02       126
## 2  2012-10-03     11352
## 3  2012-10-04     12116
## 4  2012-10-05     13294
## 5  2012-10-06     15420
## 6  2012-10-07     11015
## 7  2012-10-09     12811
## 8  2012-10-10      9900
## 9  2012-10-11     10304
## 10 2012-10-12     17382
## ..        ...       ...
```

6. Histogram of the total number of steps taken each day

```r
  hist(steps_by_day$tot_steps,col="red",xlab = "Steps", main = "Total number of steps taken each day")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 

7. Mean & Median of *Total number of steps taken per day*

```r
  steps_by_day %>% summarize(mean = mean(tot_steps), median = median(tot_steps))
```

```
## Source: local data frame [1 x 2]
## 
##       mean median
##      (dbl)  (int)
## 1 10766.19  10765
```

8. Time series plot of Average Steps Taken

```r
  avg_steps_by_interval <- activity %>% 
                              filter(!is.na(steps)) %>%
                              group_by(interval) %>% 
                              summarize(avg_steps = mean(steps))
  
  plot(avg_steps_by_interval$interval,avg_steps_by_interval$avg_steps,type = "l",xlab = "Interval", ylab = "average number of steps taken",main = "Time Series Plot of Avg Step")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png) 

9. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
  avg_steps_by_interval %>% arrange(desc(avg_steps))
```

```
## Source: local data frame [288 x 2]
## 
##    interval avg_steps
##       (int)     (dbl)
## 1       835  206.1698
## 2       840  195.9245
## 3       850  183.3962
## 4       845  179.5660
## 5       830  177.3019
## 6       820  171.1509
## 7       855  167.0189
## 8       815  157.5283
## 9       825  155.3962
## 10      900  143.4528
## ..      ...       ...
```

10. Calculate and report the total number of missing values in the dataset 

```r
    activity %>%
            filter(is.na(steps)) %>%
            summarise(rows_with_missing_value=n())
```

```
## Source: local data frame [1 x 1]
## 
##   rows_with_missing_value
##                     (int)
## 1                    2304
```

11. Seperate complate & incomplete observations into two groups

```r
  activity_complete <-  activity %>% filter(!is.na(steps)) 
  activity_NA <- activity %>% filter(is.na(steps))
```

12. Filled **NA observations**  with **Average steps taken in Interval**
i.e. Merge incomplete observations with *Average Steps By Interval*

```r
  activity_NA_filled <- merge(activity_NA,avg_steps_by_interval,by = "interval") %>%
                        select(avg_steps,date,interval)
```

13. Align variable name of *activity_NA_filled* with data frame *activity_complete* followed by rbind

```r
  names(activity_NA_filled) <- c("steps","date","interval")
  activity_NEW <- rbind(activity_complete,activity_NA_filled)
  summary(activity_NEW)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 27.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0
```

14. Cleanup temporary data frames

```r
  rm("activity_NA_filled","activity_NA","activity_complete")
```

15. Calculate the total number of steps taken per day from new dataset

```r
  steps_by_day_NEW <- activity_NEW %>% 
                        group_by(date) %>% 
                        summarize(tot_steps = sum(steps))
  steps_by_day_NEW
```

```
## Source: local data frame [61 x 2]
## 
##          date tot_steps
##        (date)     (dbl)
## 1  2012-10-01  10766.19
## 2  2012-10-02    126.00
## 3  2012-10-03  11352.00
## 4  2012-10-04  12116.00
## 5  2012-10-05  13294.00
## 6  2012-10-06  15420.00
## 7  2012-10-07  11015.00
## 8  2012-10-08  10766.19
## 9  2012-10-09  12811.00
## 10 2012-10-10   9900.00
## ..        ...       ...
```

16. Mean & Median of *Total number of steps taken per day*

```r
  steps_by_day_NEW %>% summarize(mean = mean(tot_steps), median = median(tot_steps))
```

```
## Source: local data frame [1 x 2]
## 
##       mean   median
##      (dbl)    (dbl)
## 1 10766.19 10766.19
```

17. Create a new factor variable in the dataset with two levels – weekday and weekend

```r
  weekdays1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
  activity_NEW <-mutate(activity_NEW,days = factor((weekdays(activity_NEW$date) %in% weekdays1), 
                                                   levels=c(FALSE, TRUE), 
                                                   labels=c('weekend', 'weekday')))
  
  activity_NEW
```

```
## Source: local data frame [17,568 x 4]
## 
##    steps       date interval    days
##    (dbl)     (date)    (int)  (fctr)
## 1      0 2012-10-02        0 weekday
## 2      0 2012-10-02        5 weekday
## 3      0 2012-10-02       10 weekday
## 4      0 2012-10-02       15 weekday
## 5      0 2012-10-02       20 weekday
## 6      0 2012-10-02       25 weekday
## 7      0 2012-10-02       30 weekday
## 8      0 2012-10-02       35 weekday
## 9      0 2012-10-02       40 weekday
## 10     0 2012-10-02       45 weekday
## ..   ...        ...      ...     ...
```

18. Time series plot of Average Steps Taken

```r
  library(ggplot2)
  avg_steps_by_interval_NEW <- activity_NEW %>% 
                              group_by(days,interval) %>% 
                              summarize(avg_steps = mean(steps))
  
  g <- ggplot(avg_steps_by_interval_NEW,aes(interval,avg_steps))
  g + geom_line() + labs(x="Interval") + facet_grid(. ~ days) + facet_wrap( ~ days, ncol=1) + labs(y="Average Steps") + labs(title= expression("Time Series Plot"))
```

![plot of chunk unnamed-chunk-18](figure/unnamed-chunk-18-1.png) 
