---
title: "Load Ford GoBike Data V2"
author: "Alec"
date: "9/6/2018"
output: github_document
---
list.files()
```{r, echo = FALSE}
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "Pictures/README-",
  message = FALSE
)
```
## Load Ford GoBike Data

This is an R Markdown document that downloads pubicably available Ford GoBike Data

#Load libraries
Before beginning, let's get the session info and load in some packages.
```{r sessionInfo}
sessionInfo()
```

### Load libraries

```{r LoadLibraries,echo=T,warning=FALSE,message=FALSE}
library("data.table")
library("RCurl")
library("tidyverse")
library("skimr")
library("ggplot2")
library("lubridate")
library("rjson")
```

### Helper functions
``` {r helper}
GenMatrix=function(cns,name){
  y=matrix(ncol=length(cns),nrow=0)
  colnames(y)=cns
  assign(paste(name),y,envir=.GlobalEnv)
}
```



### Load 2017 data

```{r load_2017}
FGB2017=fread("https://s3.amazonaws.com/fordgobike-data/2017-fordgobike-tripdata.csv")
```

### Load rest of the data

```{r load_2018}
for (yrmo in seq(201801,201808)){
  link=paste0("https://s3.amazonaws.com/fordgobike-data/",yrmo,"-fordgobike-tripdata.csv.zip")
  file=strsplit(link,"/")[[1]][5]
  if (!(file %in% list.files()) & url.exists(link)){
    download.file(link, dest=file, mode="wb") 
    unzip (file, exdir = "./")
    file.remove(file)
  }
  Jan2018=fread("201801-fordgobike-tripdata.csv")
  Feb2018=fread("201802-fordgobike-tripdata.csv")
  Mar2018=fread("201803-fordgobike-tripdata.csv")
  Apr2018=fread("201804-fordgobike-tripdata.csv")
  May2018=fread("201805-fordgobike-tripdata.csv")
  Jun2018=fread("201806-fordgobike-tripdata.csv")
  Jul2018=fread("201807-fordgobike-tripdata.csv")
  Aug2018=fread("201808-fordgobike-tripdata.csv")
}
```

### Combine all data into one data table.

```{r Combine}
FGB=rbindlist(list(FGB2017,Jan2018,Feb2018,Mar2018,Apr2018,May2018,Jun2018,Jul2018,Aug2018),fill=T)
```

### Attach Starting and Ending Cities

```{r GetStations}
json_file="https://gbfs.fordgobike.com/gbfs/en/station_information.json"
StationInfo=fromJSON(paste(readLines(json_file), collapse=","))
StationList=StationInfo[[3]][[1]]
unlist(lapply(StationList,function(x) length(names(x))))
GenMatrix(names(StationList[[1]]),"y")
y=as.data.table(y)
for (i in 1:length(StationList)){
  y=rbindlist(list(y,as.data.table(StationList[[i]])),fill=T)
}
Stations=y

json_file="https://gbfs.fordgobike.com/gbfs/en/system_regions.json"
SystemRegions=fromJSON(paste(readLines(json_file), collapse=","))
x=as.vector(unlist(SystemRegions$data[[1]]))
GenMatrix(c("region_id","City"),"y")
for (i in seq(1,length(x)-1,2)){
  y=rbind(y,c(x[i],x[i+1]))
}
RegionIDs=data.table(y)
mode(RegionIDs[["region_id"]])="numeric"
RegionIDs

Stations=merge(Stations,RegionIDs,by="region_id")
Stations=Stations[,!"eightd_station_services",with=F]
Stations
write.csv(Stations,"stations.csv")

origins=as.data.table(cbind(Stations[["station_id"]],Stations[["City"]]))
colnames(origins)=c("start_station_id","start city")
origins=unique(origins)
destinations=as.data.table(cbind(Stations[["station_id"]],Stations[["City"]]))
colnames(destinations)=c("end_station_id","end city")
destinations=unique(destinations)

FGB=merge(FGB,origins,by="start_station_id",all.x=TRUE,allow.cartesian=TRUE)
FGB=merge(FGB,destinations,by="end_station_id",all.x=TRUE,allow.cartesian=TRUE)
```

### Extract date data from start_time and end_time variables

```{r AttachDateStatistics}
r=as.vector(vapply(FGB[["start_time"]],function(x) strsplit(x," ")[[1]][1],character(1)))
yr=vapply(r,function(x) as.numeric(strsplit(x,"-")[[1]][1]),numeric(1))
mo=vapply(r,function(x) as.numeric(strsplit(x,"-")[[1]][2]),numeric(1))
da=vapply(r,function(x) as.numeric(strsplit(x,"-")[[1]][3]),numeric(1))
day=vapply(r,function(x) weekdays(as.Date(x)),character(1))
r2=as.vector(vapply(FGB[["start_time"]],function(x) strsplit(x," ")[[1]][2],character(1)))
se=period_to_seconds(hms(r2))
hr=vapply(r2,function(x) as.numeric(strsplit(x,split=":")[[1]][1]),numeric(1))
min=vapply(r2,function(x) as.numeric(strsplit(x,split=":")[[1]][2]),numeric(1))
sec=vapply(r2,function(x) as.numeric(strsplit(x,split=":")[[1]][3]),numeric(1))
FGB[["start_date"]]=r
FGB[["start_year"]]=yr
FGB[["start_month"]]=mo
FGB[["start_monthlabel"]]=month(mo,T)
FGB[["start_day"]]=da
FGB[["start_daytype"]]=day
FGB[["start_tsec"]]=se
FGB[["start_hr"]]=hr
FGB[["start_min"]]=min
FGB[["start_sec"]]=sec


r=as.vector(vapply(FGB[["end_time"]],function(x) strsplit(x," ")[[1]][1],character(1)))
yr=vapply(r,function(x) as.numeric(strsplit(x,"-")[[1]][1]),numeric(1))
mo=vapply(r,function(x) as.numeric(strsplit(x,"-")[[1]][2]),numeric(1))
da=vapply(r,function(x) as.numeric(strsplit(x,"-")[[1]][3]),numeric(1))
day=vapply(r,function(x) weekdays(as.Date(x)),character(1))
r2=as.vector(vapply(FGB[["end_time"]],function(x) strsplit(x," ")[[1]][2],character(1)))
se=period_to_seconds(hms(r2))
hr=vapply(r2,function(x) as.numeric(strsplit(x,split=":")[[1]][1]),numeric(1))
min=vapply(r2,function(x) as.numeric(strsplit(x,split=":")[[1]][2]),numeric(1))
sec=vapply(r2,function(x) as.numeric(strsplit(x,split=":")[[1]][3]),numeric(1))
FGB[["end_date"]]=r
FGB[["end_year"]]=yr
FGB[["end_month"]]=mo
FGB[["end_monthlabel"]]=month(mo,T)
FGB[["end_day"]]=da
FGB[["end_daytype"]]=day
FGB[["end_tsec"]]=se
FGB[["end_hr"]]=hr
FGB[["end_min"]]=min
FGB[["end_sec"]]=sec
FGB[["age"]]=2018-FGB[["member_birth_year"]]
save("FGB",file="FGB.RData")
```

```{r graph1}
FGB %>% filter(age <= 80 & user_type=="Subscriber") %>% ggplot(aes(x=age)) + geom_histogram()+xlab("Age") + 
  ylab("Count")+scale_y_continuous(name="Count", labels = scales::comma) +
  ggtitle("Subscriber Count by Age")
```

```{r graph2}
FGB %>% filter(age <= 80 & user_type=="Customer") %>% ggplot(aes(x=age)) + geom_histogram()+xlab("Age") + 
  ylab("Count")+scale_y_continuous(name="Count", labels = scales::comma) +
  ggtitle("Customer Count by Age")
```

```{r}
FGB %>% ggplot(aes(x=member_gender)) + geom_bar()+xlab("Gender")+ylab("Count") + 
  scale_y_continuous(name="Count", labels = scales::comma) +
  ggtitle("Gender Count")+scale_y_continuous(name="Count", labels = scales::comma)
```

```{r}
FGB %>% filter(age <= 80) %>% filter(member_gender!="Other") %>% ggplot(aes(x=age, color=member_gender)) + 
  geom_histogram(position="identity") + 
  facet_grid(member_gender ~ .)+
  ggtitle("Gender Histograms")+scale_y_continuous(name="Count", labels = scales::comma)
```

```{r}
FGB %>% ggplot(aes(x=start_monthlabel)) + geom_bar() + facet_grid(start_year ~ .) + 
  ggtitle("Total Rides Over Time by Month") + scale_y_continuous(name="Count", labels = scales::comma)
```

```{r}
FGB %>% ggplot(aes(x=start_daytype)) + geom_bar() + facet_grid(start_year ~ .) + 
  ggtitle("Total Rides Over Time by Daytype") + scale_y_continuous(name="Count", labels = scales::comma)
```

```{r}
FGB %>% filter(duration_sec/60<30) %>% ggplot(aes(x=duration_sec/60)) + 
  stat_ecdf()  + scale_y_continuous(name="Cumulative Proportion", labels = scales::comma) + 
  scale_x_continuous(name="Duraction in Minutes", labels = scales::comma)+
  ggtitle(("Cumulative Duration Distribution \n (below 30 minutes)"))
```

```{r}
FGB %>% filter(duration_sec/60<30) %>% ggplot(aes(x=duration_sec/60)) + 
  stat_ecdf()  + scale_y_continuous(name="Cumulative Proportion", labels = scales::comma) + 
  scale_x_continuous(name="Duraction in Minutes", labels = scales::comma)+
  ggtitle(("Cumulative Duration Distribution by Year \n (below 30 minutes)"))+  facet_grid(start_year~ .)
```

```{r graph8}
FGB %>% filter(start_hr %in% seq(0,23)) %>% ggplot(aes(x=start_hr)) + 
  geom_histogram(aes(y=..density..),position="identity") + stat_bin(bins=24)+
  scale_y_continuous(name="Count", labels = scales::comma)+
  facet_grid(start_daytype ~ .)
```

```{r graph9}
FGB %>% filter(start_hr %in% seq(0,23) & start_daytype %in% c("Saturday","Sunday")) %>% ggplot(aes(x=start_hr)) + 
  geom_histogram(aes(y=..density..),position="identity") + stat_bin(bins=24)+
  scale_y_continuous(name="Count", labels = scales::comma)+
  facet_grid(start_daytype ~ .)
```

