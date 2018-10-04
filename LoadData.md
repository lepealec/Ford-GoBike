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
## R version 3.5.0 (2018-04-23)
## Platform: x86_64-apple-darwin15.6.0 (64-bit)
## Running under: macOS High Sierra 10.13.6
## 
## Matrix products: default
## BLAS: /Library/Frameworks/R.framework/Versions/3.5/Resources/lib/libRblas.0.dylib
## LAPACK: /Library/Frameworks/R.framework/Versions/3.5/Resources/lib/libRlapack.dylib
## 
## locale:
## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
##  [1] rjson_0.2.20      lubridate_1.7.4   skimr_1.0.3      
##  [4] forcats_0.3.0     stringr_1.3.0     dplyr_0.7.6      
##  [7] purrr_0.2.5       readr_1.1.1       tidyr_0.8.1      
## [10] tibble_1.4.2      ggplot2_2.2.1     tidyverse_1.2.1  
## [13] RCurl_1.95-4.11   bitops_1.0-6      data.table_1.11.4
## 
## loaded via a namespace (and not attached):
##  [1] Rcpp_0.12.16     cellranger_1.1.0 compiler_3.5.0   pillar_1.2.2    
##  [5] plyr_1.8.4       bindr_0.1.1      tools_3.5.0      digest_0.6.15   
##  [9] jsonlite_1.5     evaluate_0.11    nlme_3.1-137     gtable_0.2.0    
## [13] lattice_0.20-35  pkgconfig_2.0.2  rlang_0.2.0      cli_1.0.0       
## [17] rstudioapi_0.7   yaml_2.2.0       haven_1.1.2      bindrcpp_0.2.2  
## [21] xml2_1.2.0       httr_1.3.1       knitr_1.20       hms_0.4.2       
## [25] rprojroot_1.3-2  grid_3.5.0       tidyselect_0.2.4 glue_1.2.0      
## [29] R6_2.2.2         readxl_1.1.0     rmarkdown_1.10   modelr_0.1.2    
## [33] magrittr_1.5     backports_1.1.2  scales_0.5.0     htmltools_0.3.6 
## [37] rvest_0.3.2      assertthat_0.2.0 colorspace_1.3-2 stringi_1.2.2   
## [41] lazyeval_0.2.1   munsell_0.4.3    broom_0.5.0      crayon_1.3.4
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
  #expand to include future months when available
```
### Combine all data into one data table.

```r
FGB=rbindlist(list(FGB2017,
                   Jan2018,Feb2018,Mar2018,
                   Apr2018,May2018,Jun2018,
                   Jul2018,Aug2018,Sep2018),fill=T)
rm(list=c('FGB2017',
          'Jan2018',
          'Feb2018',
          'Apr2018',
          'May2018',
          'Jun2018',
          'Jul2018',
          'Aug2018',
          'Sep2018'))

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
##      station_id                                         name short_name
##   1:         74                        Laguna St at Hayes St     SF-J21
##   2:         74                        Laguna St at Hayes St     SF-J21
##   3:          3 Powell St BART Station (Market St at 4th St)     SF-G27
##   4:          3 Powell St BART Station (Market St at 4th St)     SF-G27
##   5:          4                  Cyril Magnin St at Ellis St     SF-G26
##  ---                                                                   
## 634:        375                      Grove St at Masonic Ave     SF-J16
## 635:        376                       Illinois St at 20th St   SF-O30-2
## 636:        376                       Illinois St at 20th St   SF-O30-2
## 637:        377                        Fell St at Stanyan St    SF-K15-
## 638:        377                        Fell St at Stanyan St    SF-K15-
##           lat       lon region_id rental_methods capacity
##   1: 37.77643 -122.4262         3            KEY       27
##   2: 37.77643 -122.4262         3     CREDITCARD       27
##   3: 37.78638 -122.4049         3            KEY       35
##   4: 37.78638 -122.4049         3     CREDITCARD       35
##   5: 37.78588 -122.4089         3            KEY       35
##  ---                                                     
## 634: 37.77484 -122.4465        NA     CREDITCARD        0
## 635: 37.76046 -122.3875        NA            KEY        0
## 636: 37.76046 -122.3875        NA     CREDITCARD        0
## 637: 37.77189 -122.4538        NA            KEY        0
## 638: 37.77189 -122.4538        NA     CREDITCARD        0
##                                                    rental_url
##   1:  http://app.fordgobike.com/Z6gL/SiFlGfIOTF?station_id=74
##   2:  http://app.fordgobike.com/Z6gL/SiFlGfIOTF?station_id=74
##   3:   http://app.fordgobike.com/Z6gL/SiFlGfIOTF?station_id=3
##   4:   http://app.fordgobike.com/Z6gL/SiFlGfIOTF?station_id=3
##   5:   http://app.fordgobike.com/Z6gL/SiFlGfIOTF?station_id=4
##  ---                                                         
## 634: http://app.fordgobike.com/Z6gL/SiFlGfIOTF?station_id=375
## 635: http://app.fordgobike.com/Z6gL/SiFlGfIOTF?station_id=376
## 636: http://app.fordgobike.com/Z6gL/SiFlGfIOTF?station_id=376
## 637: http://app.fordgobike.com/Z6gL/SiFlGfIOTF?station_id=377
## 638: http://app.fordgobike.com/Z6gL/SiFlGfIOTF?station_id=377
##      eightd_has_key_dispenser eightd_station_services has_kiosk
##   1:                    FALSE                  <list>      TRUE
##   2:                    FALSE                  <list>      TRUE
##   3:                    FALSE                              TRUE
##   4:                    FALSE                              TRUE
##   5:                    FALSE                              TRUE
##  ---                                                           
## 634:                    FALSE                             FALSE
## 635:                    FALSE                             FALSE
## 636:                    FALSE                             FALSE
## 637:                    FALSE                             FALSE
## 638:                    FALSE                             FALSE
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
