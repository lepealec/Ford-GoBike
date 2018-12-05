---
title: "Ford GoBike Data Collection & Cleaning"
author: "Alec Lepe"
date: "09/07/2018"
output: 
  html_document:
    keep_md: true
---

## Load Ford GoBike Data
This is an R Markdown document that downloads and cleans publicly available individual trip history Ford GoBike Data.
### Session Info

```r
sessionInfo()
```

```
## R version 3.5.1 (2018-07-02)
## Platform: x86_64-w64-mingw32/x64 (64-bit)
## Running under: Windows 7 x64 (build 7601) Service Pack 1
## 
## Matrix products: default
## 
## locale:
## [1] LC_COLLATE=English_United States.1252 
## [2] LC_CTYPE=English_United States.1252   
## [3] LC_MONETARY=English_United States.1252
## [4] LC_NUMERIC=C                          
## [5] LC_TIME=English_United States.1252    
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
##  [1] rjson_0.2.20      lubridate_1.7.4   skimr_1.0.3      
##  [4] forcats_0.3.0     stringr_1.3.1     dplyr_0.7.8      
##  [7] purrr_0.2.5       readr_1.2.1       tidyr_0.8.2      
## [10] tibble_1.4.2      ggplot2_3.0.0     tidyverse_1.2.1  
## [13] RCurl_1.95-4.11   bitops_1.0-6      data.table_1.11.8
## 
## loaded via a namespace (and not attached):
##  [1] Rcpp_1.0.0       cellranger_1.1.0 plyr_1.8.4       compiler_3.5.1  
##  [5] pillar_1.3.0     bindr_0.1.1      tools_3.5.1      digest_0.6.18   
##  [9] jsonlite_1.5     evaluate_0.12    nlme_3.1-137     gtable_0.2.0    
## [13] lattice_0.20-35  pkgconfig_2.0.2  rlang_0.3.0.1    cli_1.0.1       
## [17] rstudioapi_0.8   yaml_2.2.0       haven_2.0.0      bindrcpp_0.2.2  
## [21] withr_2.1.2      xml2_1.2.0       httr_1.3.1       knitr_1.20      
## [25] hms_0.4.2        rprojroot_1.3-2  grid_3.5.1       tidyselect_0.2.5
## [29] glue_1.3.0       R6_2.3.0         readxl_1.1.0     rmarkdown_1.10  
## [33] modelr_0.1.2     magrittr_1.5     backports_1.1.2  scales_1.0.0    
## [37] htmltools_0.3.6  rvest_0.3.2      assertthat_0.2.0 colorspace_1.3-2
## [41] stringi_1.1.7    lazyeval_0.2.1   munsell_0.5.0    broom_0.5.0     
## [45] crayon_1.3.4
```
### Load libraries

```r
library("data.table")
library("RCurl")
library("tidyverse")
library("skimr")
library("ggplot2")
library("lubridate")
library("rjson")
```
### Helper functions

```r
GenMatrix=function(cns,name){
  y=matrix(ncol=length(cns),nrow=0)
  colnames(y)=cns
  assign(paste(name),y,envir=.GlobalEnv)
}
```
### Load 2017 data

```r
setwd("~/Ford-GoBike")
#fn="FGB 2017 Data.csv"
FGB2017=fread("https://s3.amazonaws.com/fordgobike-data/2017-fordgobike-tripdata.csv")
```
### Load available 2018 data

```r
setwd("~/Ford-GoBike")
for (yrmo in seq(201801,201812)){
  link=paste0("https://s3.amazonaws.com/fordgobike-data/",yrmo,"-fordgobike-tripdata.csv.zip")
  zip=strsplit(link,"/")[[1]][5]
  csv=paste(strsplit(zip,"\\.")[[1]][1:2],collapse=".")
  if (!csv %in% list.files(getwd()) & url.exists(link)){
    download.file(link, dest=zip, mode="wb") 
    unzip (zip, exdir = getwd())
    file.remove(zip)
  }
}
#  FGB2017=fread("FGB 2017 Data.csv")
  Jan2018=fread("201801-fordgobike-tripdata.csv") ;   file.remove("201801-fordgobike-tripdata.csv")
```

```
## [1] TRUE
```

```r
  Feb2018=fread("201802-fordgobike-tripdata.csv") ;   file.remove("201802-fordgobike-tripdata.csv")
```

```
## [1] TRUE
```

```r
  Mar2018=fread("201803-fordgobike-tripdata.csv") ;   file.remove("201803-fordgobike-tripdata.csv")
```

```
## [1] TRUE
```

```r
  Apr2018=fread("201804-fordgobike-tripdata.csv") ;   file.remove("201804-fordgobike-tripdata.csv")
```

```
## [1] TRUE
```

```r
  May2018=fread("201805-fordgobike-tripdata.csv") ;   file.remove("201805-fordgobike-tripdata.csv")
```

```
## [1] TRUE
```

```r
  Jun2018=fread("201806-fordgobike-tripdata.csv") ;   file.remove("201806-fordgobike-tripdata.csv")
```

```
## [1] TRUE
```

```r
  Jul2018=fread("201807-fordgobike-tripdata.csv") ;   file.remove("201807-fordgobike-tripdata.csv")
```

```
## [1] TRUE
```

```r
  Aug2018=fread("201808-fordgobike-tripdata.csv") ;   file.remove("201808-fordgobike-tripdata.csv")
```

```
## [1] TRUE
```

```r
  Sep2018=fread("201809-fordgobike-tripdata.csv") ;   file.remove("201809-fordgobike-tripdata.csv")
```

```
## [1] TRUE
```

```r
  Oct2018=fread("201810-fordgobike-tripdata.csv") ;   file.remove("201810-fordgobike-tripdata.csv")
```

```
## [1] TRUE
```

```r
  Nov2018=fread("201811-fordgobike-tripdata.csv") ;   file.remove("201811-fordgobike-tripdata.csv")
```

```
## [1] TRUE
```

```r
  #expand to include future months when available
```
### Combine all data into one data table.

```r
FGB=rbindlist(list(FGB2017,
                   Jan2018,Feb2018,Mar2018,
                   Apr2018,May2018,Jun2018,
                   Jul2018,Aug2018,Sep2018,Oct2018,Nov2018),fill=T)
rm(list=c('FGB2017',
          'Jan2018',
          'Feb2018',
          'Apr2018',
          'May2018',
          'Jun2018',
          'Jul2018',
          'Aug2018',
          'Sep2018',
          'Oct2018',
          'Nov2018'
))

#expand to include future months when available
```
### Attach Starting and Ending Cities

```r
json_file="https://gbfs.fordgobike.com/gbfs/en/station_information.json"
StationInfo=fromJSON(paste(readLines(json_file), collapse=","))
```

```
## Warning in readLines(json_file): incomplete final line found on 'https://
## gbfs.fordgobike.com/gbfs/en/station_information.json'
```

```r
StationList=StationInfo[[3]][[1]]
#unlist(lapply(StationList,function(x) length(names(x))))
GenMatrix(names(StationList[[1]]),"y")
y=as.data.table(y)
for (i in 1:length(StationList)){
  y=rbindlist(list(y,as.data.table(StationList[[i]])),fill=T)
}
Stations=y
Stations
```

```
##      station_id                          external_id
##   1:         74 83b33e8d-bf8c-4a2a-8ee3-942834af27b4
##   2:         74 83b33e8d-bf8c-4a2a-8ee3-942834af27b4
##   3:          3 1b13a386-c5f4-42cc-bc3b-ded95982e090
##   4:          3 1b13a386-c5f4-42cc-bc3b-ded95982e090
##   5:          4 a00d04e6-0159-466a-b3ab-23f9550f418c
##  ---                                                
## 640:        380 175541bb-0ac6-42ba-bb50-071317d1237b
## 641:        381 2c3d0e1d-f278-48df-b887-4723d4bcbefe
## 642:        381 2c3d0e1d-f278-48df-b887-4723d4bcbefe
## 643:        383 63758c7b-dfd9-4548-92a2-28fe8efb2faa
## 644:        383 63758c7b-dfd9-4548-92a2-28fe8efb2faa
##                                              name short_name      lat
##   1:                        Laguna St at Hayes St     SF-J21 37.77643
##   2:                        Laguna St at Hayes St     SF-J21 37.77643
##   3: Powell St BART Station (Market St at 4th St)     SF-G27 37.78638
##   4: Powell St BART Station (Market St at 4th St)     SF-G27 37.78638
##   5:                  Cyril Magnin St at Ellis St     SF-G26 37.78588
##  ---                                                                 
## 640:                       Masonic Ave at Turk St     SF-I16 37.77919
## 641:                        20th St at Dolores St     SF-P21 37.75824
## 642:                        20th St at Dolores St     SF-P21 37.75824
## 643:               Golden Gate Ave at Franklin St     SF-I22 37.78079
## 644:               Golden Gate Ave at Franklin St     SF-I22 37.78079
##            lon region_id rental_methods capacity
##   1: -122.4262         3            KEY       27
##   2: -122.4262         3     CREDITCARD       27
##   3: -122.4049         3            KEY       35
##   4: -122.4049         3     CREDITCARD       35
##   5: -122.4089         3            KEY       35
##  ---                                            
## 640: -122.4473        NA     CREDITCARD        0
## 641: -122.4261         3            KEY       27
## 642: -122.4261         3     CREDITCARD       27
## 643: -122.4219        NA            KEY        0
## 644: -122.4219        NA     CREDITCARD        0
##                                                    rental_url
##   1:  http://app.fordgobike.com/Z6gL/SiFlGfIOTF?station_id=74
##   2:  http://app.fordgobike.com/Z6gL/SiFlGfIOTF?station_id=74
##   3:   http://app.fordgobike.com/Z6gL/SiFlGfIOTF?station_id=3
##   4:   http://app.fordgobike.com/Z6gL/SiFlGfIOTF?station_id=3
##   5:   http://app.fordgobike.com/Z6gL/SiFlGfIOTF?station_id=4
##  ---                                                         
## 640: http://app.fordgobike.com/Z6gL/SiFlGfIOTF?station_id=380
## 641: http://app.fordgobike.com/Z6gL/SiFlGfIOTF?station_id=381
## 642: http://app.fordgobike.com/Z6gL/SiFlGfIOTF?station_id=381
## 643: http://app.fordgobike.com/Z6gL/SiFlGfIOTF?station_id=383
## 644: http://app.fordgobike.com/Z6gL/SiFlGfIOTF?station_id=383
##      eightd_has_key_dispenser eightd_station_services has_kiosk
##   1:                    FALSE                  <list>      TRUE
##   2:                    FALSE                  <list>      TRUE
##   3:                    FALSE                              TRUE
##   4:                    FALSE                              TRUE
##   5:                    FALSE                              TRUE
##  ---                                                           
## 640:                    FALSE                             FALSE
## 641:                    FALSE                              TRUE
## 642:                    FALSE                              TRUE
## 643:                    FALSE                             FALSE
## 644:                    FALSE                             FALSE
```

```r
json_file="https://gbfs.fordgobike.com/gbfs/en/system_regions.json"
SystemRegions=fromJSON(paste(readLines(json_file), collapse=","))
```

```
## Warning in readLines(json_file): incomplete final line found on 'https://
## gbfs.fordgobike.com/gbfs/en/system_regions.json'
```

```r
x=as.vector(unlist(SystemRegions$data[[1]]))
GenMatrix(c("region_id","City"),"y")
for (i in seq(1,length(x)-1,2)){
  y=rbind(y,c(x[i],x[i+1]))
}
RegionIDs=data.table(y)
mode(RegionIDs[["region_id"]])="numeric"
RegionIDs
```

```
##    region_id          City
## 1:         3 San Francisco
## 2:         5      San Jose
## 3:        12       Oakland
## 4:        13    Emeryville
## 5:        14      Berkeley
## 6:        23            8D
```

```r
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

```r
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
save("FGB",file="FGB.RData")
```
