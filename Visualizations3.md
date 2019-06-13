---
title: "Ford GoBike Data Visualization3"
author: "Alec Lepe"
date: "06/12/2018"
output: 
  html_document:
    keep_md: true
---


### Load Data

```r
setwd("~/Desktop/Projects/Ford-GoBike/Clean Data")
load("FGB.RData")
```

```r
FGB %>% ggplot(aes(x=StartMonthLabel)) + geom_bar() + facet_grid(StartYear ~ .) + 
  xlab("Month") + scale_y_continuous(name="Count", labels = scales::comma) +
  ggtitle("Total Rides Over Time by Month")
```

![](Visualizations3_files/figure-html/graph5-1.png)<!-- -->


```r
berk=FGB[FGB[["StartCity"]]=="Berkeley" & FGB[["EndCity"]]=="Berkeley",]
berk %>% ggplot(aes(x=StartMonthLabel)) + geom_bar() + facet_grid(StartYear ~ .) +xlab("Month") + scale_y_continuous(name="Count", labels = scales::comma) +
  ggtitle("Total Rides Over Time by Month\nBerkeley")
```

![](Visualizations3_files/figure-html/graph5a-1.png)<!-- -->

```r
sanjose=FGB[FGB[["StartCity"]]=="San Jose" & FGB[["EndCity"]]=="San Jose",]
sanjose %>% ggplot(aes(x=StartMonthLabel)) + geom_bar() + facet_grid(StartYear ~ .) +xlab("Month") + scale_y_continuous(name="Count", labels = scales::comma) +
  ggtitle("Total Rides Over Time by Month\nSan Jose")
```

![](Visualizations3_files/figure-html/graph5b-1.png)<!-- -->

```r
Oakland=FGB[FGB[["StartCity"]]=="Oakland" & FGB[["EndCity"]]=="Oakland",]
Oakland %>% ggplot(aes(x=StartMonthLabel)) + geom_bar() + facet_grid(StartYear ~ .) +xlab("Month") + scale_y_continuous(name="Count", labels = scales::comma) +
  ggtitle("Total Rides Over Time by Month\nOakland")
```

![](Visualizations3_files/figure-html/graph5c-1.png)<!-- -->

```r
SF=FGB[FGB[["StartCity"]]=="San Francisco" & FGB[["EndCity"]]=="San Francisco",]
SF %>% ggplot(aes(x=StartMonthLabel)) + geom_bar() + facet_grid(StartYear ~ .) +xlab("Month") + scale_y_continuous(name="Count", labels = scales::comma) +
  ggtitle("Total Rides Over Time by Month \nSan Francisco")
```

![](Visualizations3_files/figure-html/graph5d-1.png)<!-- -->

```r
Emeryville=FGB[FGB[["StartCity"]]=="Emeryville" & FGB[["EndCity"]]=="Emeryville",]
Emeryville %>% ggplot(aes(x=StartMonthLabel)) + geom_bar() + facet_grid(StartYear ~ .) +xlab("Month") + scale_y_continuous(name="Count", labels = scales::comma) +
  ggtitle("Total Rides Over Time by Month \nEmeryville")
```

![](Visualizations3_files/figure-html/graph5e-1.png)<!-- -->
