library("data.table")
library("RCurl")
library("tidyverse")
library("skimr")
library("ggplot2")
library("lubridate")
library("rjson")
for (yrmo in (201812)){
  #yrmo=201812
  link=paste0("https://s3.amazonaws.com/fordgobike-data/",yrmo,"-fordgobike-tripdata.csv.zip")
  zip=strsplit(link,"/")[[1]][5]
  csv=paste(strsplit(zip,"\\.")[[1]][1:2],collapse=".")
  if (!csv %in% list.files(getwd()) & url.exists(link)){
    download.file(link, dest=zip, mode="wb") 
    unzip (zip, exdir = getwd())
    file.remove(zip)
  }
}
Dec2018=fread("201812-fordgobike-tripdata.csv") ;   file.remove("201812-fordgobike-tripdata.csv")
#expand to include future months when available
```
### Combine all data into one data table.
```{r Combine}
FGB=Dec2018
json_file="https://gbfs.fordgobike.com/gbfs/en/station_information.json"
StationInfo=fromJSON(paste(readLines(json_file), collapse=","))
StationList=StationInfo[[3]][[1]]
#unlist(lapply(StationList,function(x) length(names(x))))
GenMatrix=function(cns,name){
  y=matrix(ncol=length(cns),nrow=0)
  colnames(y)=cns
  assign(paste(name),y,envir=.GlobalEnv)
}
GenMatrix(names(StationList[[1]]),"y")
y=as.data.table(y)
for (i in 1:length(StationList)){
  y=rbindlist(list(y,as.data.table(StationList[[i]])),fill=T)
}
Stations=y
Stations

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
setwd("~/Ford-GoBike/Clean Data")
write.csv(Stations,"stations.csv")

origins=as.data.table(cbind(Stations[["station_id"]],Stations[["City"]]))
colnames(origins)=c("start_station_id","start city")
origins=unique(origins)
destinations=as.data.table(cbind(Stations[["station_id"]],Stations[["City"]]))
colnames(destinations)=c("end_station_id","end city")
destinations=unique(destinations)

FGB=merge(FGB,origins,by="start_station_id",all.x=TRUE,allow.cartesian=TRUE)
FGB=merge(FGB,destinations,by="end_station_id",all.x=TRUE,allow.cartesian=TRUE)
FGB[["age"]]=as.numeric(strsplit(strsplit(as.character(now()),split=" ")[[1]][1],"-")[[1]][1])-FGB[["member_birth_year"]]
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
setwd("~/Ford-GoBike/Clean Data")
x=FGB
load("FGB.RData")
FGB2=rbindlist(list(FGB,x),fill=T)
FGB=FGB2
save("FGB",file="FGB.RData")
```
