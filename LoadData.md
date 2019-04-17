---
title: "Ford GoBike Load Data"
author: "Alec Lepe"
date: "4/16/2018"
output: 
  html_document:
    keep_md: true
---







### Load Data


### Generate Station Library and Attach Starting and Ending Cities to FGB

```
## Warning in readLines(json_file): incomplete final line found on 'https://
## gbfs.fordgobike.com/gbfs/en/station_information.json'
```

```
## Warning in ann(Stations): NAs introduced by coercion

## Warning in ann(Stations): NAs introduced by coercion

## Warning in ann(Stations): NAs introduced by coercion

## Warning in ann(Stations): NAs introduced by coercion

## Warning in ann(Stations): NAs introduced by coercion

## Warning in ann(Stations): NAs introduced by coercion

## Warning in ann(Stations): NAs introduced by coercion

## Warning in ann(Stations): NAs introduced by coercion
```

```
## Warning in readLines(json_file): incomplete final line found on 'https://
## gbfs.fordgobike.com/gbfs/en/system_regions.json'
```

```
## Warning in `[.data.table`(Stations, , !"eightd_station_services", with =
## F): column(s) not removed because not found: eightd_station_services
```

### Extract date data from start_time and end_time variables


