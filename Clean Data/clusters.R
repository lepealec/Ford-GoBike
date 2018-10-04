setwd("~/Ford-GoBike/Clean Data")
library("tidyverse")
library("skimr")
library("ggplot2")
library("data.table")
library("lubridate")
load("FGB.RData")
#install.packages("ggmap")
#install.packages('biganalytics')
library('biganalytics')
library("ggmap")

colnames(FGB)[colnames(FGB)=="start city"]="start_city"
colnames(FGB)[colnames(FGB)=="end city"]="end_city"
FGB_lon_lat=FGB %>% select(start_station_name,start_city,start_station_longitude,start_station_latitude)
FGB_lon_lat=unique(FGB_lon_lat)
FGB_lon_lat=FGB_lon_lat[FGB_lon_lat[["start_station_name"]]!="NULL",]
FGB_lon_lat=FGB_lon_lat[!is.na(FGB_lon_lat[["start_city"]]),]
#Number of stations per city
sort(table(FGB_lon_lat[[2]]),T)


FGB_lon_lat %>% ggplot(aes(x=start_station_longitude, y=start_station_latitude)) +
  geom_point()
vc=c("Berkeley","Emeryville","Oakland")
vc=c("Berkeley")

x=
FGB[
#  FGB[["start_year"]]==2018 &
#  FGB[["start_monthlabel"]]=="Aug" &
#  FGB[["start_day"]]==6 & 
  FGB[["start_city"]] %in% vc  & 
  FGB[["end_city"]] %in% vc
  ,]
if (type=="eb"){
  zo=12
} else if (type=="Berkeley") {
  zo=13
}
map <- get_map(location = c(lon=mean(x[["start_station_longitude"]]), lat=mean(x[["start_station_latitude"]])), zoom = zo, maptype = "roadmap")
ggmap(map) +
  geom_point(data = x, aes(x = start_station_longitude, y = start_station_latitude), size = 0.8, shape = 19) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()
  )
ggsave(filename="Berkeley.jpg", width = 4, height = 4, units = "cm", plot=last_plot())
x[[""]]
geom_line()
x[["od"]]=paste(x[["start_station_name"]],x[["end_station_name"]])
r=as.matrix(table(x[["od"]]))
r=cbind(r,rownames(r))
colnames(r)[1:2]=c("Freq","od")
r=as.data.table(r)
x=merge(x,r,"od",all.x=T)
mode(x[["Freq"]])="numeric"
ggmap(map) +
  geom_point(data = x, aes(x = start_station_longitude, y = start_station_latitude), size = 0.8, shape = 19) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()
  )+geom_segment(data=x,aes(
                x=x[["start_station_longitude"]],
                y=x[["start_station_latitude"]],
               xend=x[["end_station_longitude"]],
               yend=x[["end_station_latitude"]]),size=x[["Freq"]]/100)


####






