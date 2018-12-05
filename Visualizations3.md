---
title: "Ford GoBike Data Visualization3"
author: "Alec Lepe"
date: "09/07/2018"
output: 
  html_document:
    keep_md: true
---


### Load Data

```r
setwd("~/Ford-GoBike/Clean Data")
load("FGB.RData")
```

```r
FGB %>% ggplot(aes(x=start_monthlabel)) + geom_bar() + facet_grid(start_year ~ .) + 
  xlab("Month") + scale_y_continuous(name="Count", labels = scales::comma) +
  ggtitle("Total Rides Over Time by Month")
```

![](Visualizations3_files/figure-html/graph5-1.png)<!-- -->


```r
berk=FGB[FGB[["start city"]]=="Berkeley" & FGB[["end city"]]=="Berkeley",]
berk %>% ggplot(aes(x=start_monthlabel)) + geom_bar() + facet_grid(start_year ~ .) +xlab("Month") + scale_y_continuous(name="Count", labels = scales::comma) +
  ggtitle("Total Rides Over Time by Month\nBerkeley")
```

![](Visualizations3_files/figure-html/graph5a-1.png)<!-- -->

```r
sanjose=FGB[FGB[["start city"]]=="San Jose" & FGB[["end city"]]=="San Jose",]
sanjose %>% ggplot(aes(x=start_monthlabel)) + geom_bar() + facet_grid(start_year ~ .) +xlab("Month") + scale_y_continuous(name="Count", labels = scales::comma) +
  ggtitle("Total Rides Over Time by Month\nSan Jose")
```

![](Visualizations3_files/figure-html/graph5b-1.png)<!-- -->

```r
Oakland=FGB[FGB[["start city"]]=="Oakland" & FGB[["end city"]]=="Oakland",]
Oakland %>% ggplot(aes(x=start_monthlabel)) + geom_bar() + facet_grid(start_year ~ .) +xlab("Month") + scale_y_continuous(name="Count", labels = scales::comma) +
  ggtitle("Total Rides Over Time by Month\nOakland")
```

![](Visualizations3_files/figure-html/graph5c-1.png)<!-- -->

```r
SF=FGB[FGB[["start city"]]=="San Francisco" & FGB[["end city"]]=="San Francisco",]
SF %>% ggplot(aes(x=start_monthlabel)) + geom_bar() + facet_grid(start_year ~ .) +xlab("Month") + scale_y_continuous(name="Count", labels = scales::comma) +
  ggtitle("Total Rides Over Time by Month \nSan Francisco")
```

![](Visualizations3_files/figure-html/graph5d-1.png)<!-- -->

```r
Emeryville=FGB[FGB[["start city"]]=="Emeryville" & FGB[["end city"]]=="Emeryville",]
Emeryville %>% ggplot(aes(x=start_monthlabel)) + geom_bar() + facet_grid(start_year ~ .) +xlab("Month") + scale_y_continuous(name="Count", labels = scales::comma) +
  ggtitle("Total Rides Over Time by Month \nEmeryville")
```

![](Visualizations3_files/figure-html/graph5e-1.png)<!-- -->
