library(leaflet)
library("tidyverse")


colnames(FGB)[colnames(FGB)=="start city"]="start_city"
colnames(FGB)[colnames(FGB)=="end city"]="end_city"

vc=c("Berkeley")
type="Berkeley"

FGB_lon_lat=FGB %>% select(start_station_name,start_city,start_station_longitude,start_station_latitude,start_station_id)
FGB_lon_lat=unique(FGB_lon_lat)
FGB_lon_lat=FGB_lon_lat[FGB_lon_lat[["start_station_name"]]!="NULL",]
FGB_lon_lat=FGB_lon_lat[!is.na(FGB_lon_lat[["start_city"]]),]
FGB_lon_lat=FGB_lon_lat[!duplicated(FGB_lon_lat[["start_station_id"]]),]

Berkeley=FGB_lon_lat[FGB_lon_lat[["start_city"]] %in% c("Berkeley"),]

y=FGB_lon_lat[FGB_lon_lat[["start_city"]] %in% "Berkeley",]
x=x <- leaflet() %>%  addTiles() 
for (i in seq(1,nrow(y))){
  x=x %>%    addMarkers(lng=y[i,][["start_station_longitude"]], 
               lat=y[i,][["start_station_latitude"]], 
               popup=paste(y[i,][["start_station_name"]],"ID:",y[i,][["start_station_id"]]))
}
berk=x

y=FGB_lon_lat[FGB_lon_lat[["start_city"]] %in% "San Jose",]
x=x <- leaflet() %>%  addTiles() 
for (i in seq(1,nrow(y))){
  x=x %>%    addMarkers(lng=y[i,][["start_station_longitude"]], 
               lat=y[i,][["start_station_latitude"]], 
               popup=paste(y[i,][["start_station_name"]],"ID:",y[i,][["start_station_id"]]))
}
sj=x
y=FGB_lon_lat[FGB_lon_lat[["start_city"]] %in% "Oakland",]
x=x <- leaflet() %>%  addTiles() 
for (i in seq(1,nrow(y))){
  x=x %>%    addMarkers(lng=y[i,][["start_station_longitude"]], 
                        lat=y[i,][["start_station_latitude"]], 
                        popup=paste(y[i,][["start_station_name"]],"ID:",y[i,][["start_station_id"]]))
}
oak=x
y=FGB_lon_lat[FGB_lon_lat[["start_city"]] %in% "Emeryville",]
x=x <- leaflet() %>%  addTiles() 
for (i in seq(1,nrow(y))){
  x=x %>%    addMarkers(lng=y[i,][["start_station_longitude"]], 
                        lat=y[i,][["start_station_latitude"]], 
                        popup=paste(y[i,][["start_station_name"]],"ID:",y[i,][["start_station_id"]]))
}
emery=x
y=FGB_lon_lat[FGB_lon_lat[["start_city"]] %in% "San Francisco",]
x=x <- leaflet() %>%  addTiles() 
for (i in seq(1,nrow(y))){
  x=x %>%    addMarkers(lng=y[i,][["start_station_longitude"]], 
                        lat=y[i,][["start_station_latitude"]], 
                        popup=paste(y[i,][["start_station_name"]],"ID:",y[i,][["start_station_id"]]))
}
sf=x

y=FGB_lon_lat
x=x <- leaflet() %>%  addTiles() 
for (i in seq(1,nrow(y))){
  x=x %>%    addMarkers(lng=y[i,][["start_station_longitude"]], 
                        lat=y[i,][["start_station_latitude"]], 
                        popup=paste(y[i,][["start_station_name"]],"ID:",y[i,][["start_station_id"]]))
}
all=x

save(all,sf,emery,oak,berk,sj,file="maps.RData")
