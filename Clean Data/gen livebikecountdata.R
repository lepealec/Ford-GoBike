rm(list=ls())
library(shiny)
library(DT)
library(data.table)
library(leaflet)
library(tidyverse)
library("RCurl")
library("rvest")
library("jsonlite")
setwd("~/Ford-GoBIke/Clean Data")
url=paste0('https://gbfs.fordgobike.com/gbfs/en/station_information.json')
a=read_json(url)
stations=a[[3]]$stations
st=matrix(ncol=length(names(unlist(stations[[1]]))),nrow=0)
colnames(st)=names(unlist(stations[[1]]))
for (i in seq(1,length(stations))){
  st=rbind(st,as.vector(unlist(stations[[i]])))
}
st=as.data.table(st)
st=st[st[["rental_url"]]!="FALSE",]
st[["city"]]=unlist(lapply(st[["short_name"]],function(x) paste0(strsplit(x,"")[[1]][1:2],collapse="")))
st[st[["city"]]=="BK",][["city"]]="Berkeley"
st[st[["city"]]=="OK",][["city"]]="Oakland"
st[st[["city"]]=="SJ",][["city"]]="San Jose"
st[st[["city"]]=="SF",][["city"]]="San Francisco"
st[st[["city"]]=="EM",][["city"]]="Emeryville"
st=st[,!"external_id"]
st=st[,!"region_id"]
st=st[,!"eightd_station_services.docks_availability"]
st=st[,!"rental_methods1"]
st=st[,!"has_kiosk"]
st=st[,!"eightd_station_services.link_for_more_info"]
st=st[,!"eightd_station_services.service_type"]
st=st[,!"rental_methods2"]
st=st[,!"rental_url"]
st=st[,!"eightd_has_key_dispenser"]
st=st[,!"eightd_station_services.id"]
st=st[,!"eightd_station_services.schedule_description"]
st=st[,!"eightd_station_services.bikes_availability"]
st=st[,!"eightd_station_services.name"]
st=st[,!"eightd_station_services.description"]
st1=st
ac=function(x){
  as.character(x)
}
ul=function(x){
  unlist(x)
}
nr=function(x){
  nrow(x)
}
an=function(x){
  as.numeric(x)
}
adt=function(x){
  as.data.table(x)
}

save.image("livebikecount.RData")


url=paste0("https://gbfs.fordgobike.com/gbfs/en/station_status.json")
fn=paste0(paste0(strsplit(ac(Sys.time()),split=" ")[[1]][1]," ",
                 paste0(strsplit(strsplit(ac(Sys.time()),split=" ")[[1]][2],split=":")[[1]],collapse="."),collapse=" ")
)
a=read_json(url)
stations=a[[3]]$stations
names=lapply(stations,function(x) names(ul(x)))
names=unique(names)
nz=names[which(ul(lapply(names,length))==max(ul(lapply(names,length))))]
st=matrix(ncol=length(nz[[1]]),nrow=0)
colnames(st)=nz[[1]]
for (i in seq(1,length(stations))){
  y=(ul(stations[[i]]))
  st=rbind(st,rep("NA",ncol(st)))
  st[nr(st),names(y)]=y
}
st=as.data.table(st)
m=merge(st,st1,all.x=T)
m=m[!is.na(m[["city"]]),]

t1=m[,sum(an(is_renting)),city];t2=m[,length(an(is_renting)),city]
c1="Active Stations";c2="Total Stations"
colnames(t1)[2]=c1;colnames(t2)[2]=c2
t3=merge(t1,t2)
t3=rbindlist(list(t3,adt(t(adt(c("All",sum(t3[[c1]]),sum(t3[[c2]])))))))
mode(t3[[2]])="numeric" ; mode(t3[[3]])="numeric" 
t3[["Percentage"]]=round(t3[[c1]]/t3[[c2]],2)
colnames(t3)[1]='City'
active_station_count=t3

t1=m[,sum(an(num_bikes_available)),city];t2=m[,sum(an(num_ebikes_available)),city]
c1="Available Bikes";c2="Available eBikes"
colnames(t1)[2]=c1;colnames(t2)[2]=c2
t3=merge(t1,t2)

t1=m[,sum(an(num_docks_available)),city];t2=m[,sum(an(num_bikes_disabled)),city]
c1="Available Docks";c2="Disabled Bikes"
colnames(t1)[2]=c1;colnames(t2)[2]=c2
t3=merge(t3,t1);t3=merge(t3,t2)

t1=m[,sum(an(num_docks_disabled)),city];t2=m[,sum(an(is_installed)),city]
c1="Disabled Docks";c2="Installed Stations"
colnames(t1)[2]=c1;colnames(t2)[2]=c2
t3=merge(t3,t1);t3=merge(t3,t2)

t1=m[,sum(an(is_renting)),city];t2=m[,sum(an(is_returning)),city]
c1="Renting Stations";c2="Returning Stations"
colnames(t1)[2]=c1;colnames(t2)[2]=c2
t3=merge(t3,t1);t3=merge(t3,t2)

t3=rbindlist(list(t3,adt(t(adt(c("All",
                                 sum(t3[[2]]),
                                 sum(t3[[3]]),
                                 sum(t3[[4]]),
                                 sum(t3[[5]]),
                                 sum(t3[[6]]),
                                 sum(t3[[7]]),
                                 sum(t3[[8]]),
                                 sum(t3[[9]])
                                 ))))))


m[["last_reported"]]=anytime(as.numeric(m[["last_reported"]]))
m=m[,!"eightd_active_station_services.id"]
m=m[,!"eightd_has_available_keys"]

