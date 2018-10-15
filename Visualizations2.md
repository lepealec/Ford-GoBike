---
title: "Ford GoBike Data Visualization"
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
abs=table(FGB[["start city"]][!is.na(FGB[["start city"]]) & !is.na(FGB[["end city"]])],
FGB[["end city"]][!is.na(FGB[["start city"]]) & !is.na(FGB[["end city"]])])
abs=cbind(abs,"Origin Total"=apply(abs,1,sum))
abs=rbind(abs,"Destination Total"=apply(abs,2,sum))
abs[,]=prettyNum(abs,",")
abs
```

```
##                   Berkeley  Emeryville Oakland   San Francisco San Jose
## Berkeley          "94,072"  "4,652"    "16,549"  "13"          "0"     
## Emeryville        "3,144"   "5,527"    "6,572"   "15"          "0"     
## Oakland           "12,760"  "8,187"    "237,956" "68"          "2"     
## San Francisco     "8"       "5"        "26"      "1,392,448"   "1"     
## San Jose          "0"       "0"        "0"       "1"           "81,536"
## Destination Total "109,984" "18,371"   "261,103" "1,392,545"   "81,539"
##                   Origin Total
## Berkeley          "115,286"   
## Emeryville        "15,258"    
## Oakland           "258,973"   
## San Francisco     "1,392,488" 
## San Jose          "81,537"    
## Destination Total "1,863,542"
```
Note: Rows=Origins, Columns=Destinations

<!-- #sum(na.omit(FGB[["start city"]]=="Berkeley" & FGB[["end city"]]=="Emeryville")) -->
<!-- #Rows=Origins -->
<!-- #Columns=Destination -->


```r
options(scipen=999)
rel=round(table(FGB[["start city"]][!is.na(FGB[["start city"]]) & !is.na(FGB[["end city"]])],
      FGB[["end city"]][!is.na(FGB[["start city"]]) & !is.na(FGB[["end city"]])])/sum(
        table(FGB[["start city"]][!is.na(FGB[["start city"]]) & !is.na(FGB[["end city"]])],
              FGB[["end city"]][!is.na(FGB[["start city"]]) & !is.na(FGB[["end city"]])])
      ),3)
rel=cbind(rel,"Origin Total"=apply(rel,1,sum))
rel=rbind(rel,"Destination Total"=apply(rel,2,sum))
rel
```

```
##                   Berkeley Emeryville Oakland San Francisco San Jose
## Berkeley             0.050      0.002   0.009         0.000    0.000
## Emeryville           0.002      0.003   0.004         0.000    0.000
## Oakland              0.007      0.004   0.128         0.000    0.000
## San Francisco        0.000      0.000   0.000         0.747    0.000
## San Jose             0.000      0.000   0.000         0.000    0.044
## Destination Total    0.059      0.009   0.141         0.747    0.044
##                   Origin Total
## Berkeley                 0.061
## Emeryville               0.009
## Oakland                  0.139
## San Francisco            0.747
## San Jose                 0.044
## Destination Total        1.000
```
Note: Rows=Origins, Columns=Destinations

2 Dimension Filter Functions

```r
filter_data=function(dt=FGB,var1,var2){
  dt=dt[!is.na(dt[[var1]]) & !is.na(dt[[var2]]) & dt[[var1]]!="NULL" & dt[[var2]]!="NULL"]
  return(dt)
}
table_data=function(dt=FGB,var1,var2,col1,col2,select1,select2){
  out=paste(
    dt[dt[[col1]]==select1][[var1]],
    dt[dt[[col2]]==select2][[var2]],sep=" , ")
  return(table(out))
}
```

### Popular ods by City

```r
ss=filter_data(FGB,"start_station_name","end_station_name")
x1=as.matrix(sort(table_data(ss,"start_station_name","end_station_name","start city","end city","San Francisco","San Francisco"),decreasing=T)[1:10])
rownames(x1)[10]=paste0(strsplit(rownames(x1)[10],"")[[1]][1:(length(strsplit(rownames(x1)[10],"")[[1]])-24)],collapse="")
x2=as.matrix(sort(table_data(ss,"start_station_name","end_station_name","start city","end city","Oakland","Oakland"),decreasing=T)[1:10])
x3=as.matrix(sort(table_data(ss,"start_station_name","end_station_name","start city","end city","Berkeley","Berkeley"),decreasing=T)[1:10])
x4=as.matrix(sort(table_data(ss,"start_station_name","end_station_name","start city","end city","San Jose","San Jose"),decreasing=T)[1:10])
x5=as.matrix(sort(table_data(ss,"start_station_name","end_station_name","start city","end city","Emeryville","Emeryville"),decreasing=T)[1:10])
ods=cbind(rownames(x1),x1,
              rownames(x2),x2,
              rownames(x3),x3,
              rownames(x4),x4,
              rownames(x5),x5)
rownames(ods)=NULL
colnames(ods)=c(rep("San Francisco",2),rep("Oakland",2),rep("Berkeley",2),rep("san Jose",2),rep("Emeryville",2))
colnames(x1)="San Francisco";colnames(x2)="Oakland";colnames(x3)="Berkeley";colnames(x4)="San Jose";colnames(x5)="Emeryville"
x1;x2;x3;x4;x5
```

```
##                                                                                    San Francisco
## San Francisco Ferry Building (Harry Bridges Plaza) , The Embarcadero at Sansome St          9599
## The Embarcadero at Sansome St , Steuart St at Market St                                     5516
## Berry St at 4th St , San Francisco Ferry Building (Harry Bridges Plaza)                     5237
## The Embarcadero at Sansome St , San Francisco Ferry Building (Harry Bridges Plaza)          4729
## San Francisco Ferry Building (Harry Bridges Plaza) , Berry St at 4th St                     3954
## Steuart St at Market St , The Embarcadero at Sansome St                                     3890
## The Embarcadero at Sansome St , The Embarcadero at Sansome St                               3351
## Howard St at Beale St , San Francisco Caltrain (Townsend St at 4th St)                      3220
## Market St at 10th St , San Francisco Caltrain Station 2  (Townsend St at 4th St)            3210
## Montgomery St BART Station (Market St at 2nd St) , San Francisco Caltrain                   3021
```

```
##                                                         Oakland
## Grand Ave at Perkins St , 19th Street BART Station         4224
## 19th Street BART Station , Bay Pl at Vernon St             4209
## Bay Pl at Vernon St , 19th Street BART Station             4044
## Lake Merritt BART Station , El Embarcadero at Grand Ave    2394
## El Embarcadero at Grand Ave , Grand Ave at Perkins St      1908
## 19th Street BART Station , Genoa St at 55th St             1907
## Broadway at 40th St , MacArthur BART Station               1885
## MacArthur BART Station , Webster St at 2nd St              1763
## 2nd Ave at E 18th St , College Ave at Harwood Ave          1723
## 2nd Ave at E 18th St , 2nd Ave at E 18th St                1719
```

```
##                                                             Berkeley
## Bancroft Way at College Ave , Fulton St at Bancroft Way         2866
## Bancroft Way at Telegraph Ave , Berkeley Civic Center           2217
## Bancroft Way at College Ave , Milvia St at Derby St             1890
## College Ave at Alcatraz Ave , Bancroft Way at Telegraph Ave     1196
## Derby St at College Ave , Bancroft Way at Telegraph Ave         1184
## Downtown Berkeley BART , Bancroft Way at College Ave            1097
## Downtown Berkeley BART , Downtown Berkeley BART                 1055
## Ashby BART Station , Haste St at Telegraph Ave                   981
## Bancroft Way at Telegraph Ave , Ashby BART Station               981
## Telegraph Ave at Ashby Ave , Downtown Berkeley BART              968
```

```
##                                                           San Jose
## 5th St at Virginia St , San Fernando at 7th St                1193
## San Fernando St at 4th St , Ryland Park                        813
## San Fernando at 7th St , 5th St at Virginia St                 808
## Ryland Park , San Fernando St at 4th St                        752
## San Jose Diridon Station , San Fernando St at 4th St           724
## 5th St at Virginia St , San Salvador St at 9th St              713
## 5th St at Virginia St , San Fernando St at 4th St              670
## San Fernando St at 4th St , 5th St at Virginia St              647
## San Jose Diridon Station , Santa Clara St at Almaden Blvd      613
## Ryland Park , San Fernando at 7th St                           601
```

```
##                                                  Emeryville
## Horton St at 40th St , 65th St at Hollis St             793
## Horton St at 40th St , Horton St at 40th St             660
## Adeline St at 40th St , 47th St at San Pablo Ave        598
## Emeryville Town Hall , 65th St at Hollis St             501
## 59th St at Horton St , Adeline St at 40th St            437
## Adeline St at 40th St , 65th St at Hollis St            431
## Emeryville Town Hall , Horton St at 40th St             399
## Adeline St at 40th St , 59th St at Horton St            395
## 47th St at San Pablo Ave , 65th St at Hollis St         375
## 65th St at Hollis St , Emeryville Town Hall             366
```

```r
#as.data.table(ods)
```

1 Dimension Filter Functions

```r
filter_data_1d=function(dt=FGB,var1,col=c(),vars=c(),not=T){
  if (length(vars)==0 & length(col)==0){
    dt=dt[!is.na(dt[[var1]]) & dt[[var1]]!="NULL"]
  } else{
    if (not){
      dt=dt[!is.na(dt[[var1]]) & dt[[var1]]!="NULL" & !(dt[[col]] %in% vars)]
    } else {
      dt=dt[!is.na(dt[[var1]]) & dt[[var1]]!="NULL" & (dt[[col]] %in% vars)]
    }
  }
  return(dt)
}
table_data_1d=function(dt=FGB,var1,col1,select1){
  out=paste(
    dt[dt[[col1]]==select1][[var1]])
  return(table(out))
}
```

### Popular Origins by City

```r
ss=filter_data_1d(FGB,"start_station_name")

x1=as.matrix(sort(table_data_1d(ss,"start_station_name","start city","San Francisco"),decreasing=T)[1:10])
x2=as.matrix(sort(table_data_1d(ss,"start_station_name","start city","Oakland"),decreasing=T)[1:10])
x3=as.matrix(sort(table_data_1d(ss,"start_station_name","start city","Berkeley"),decreasing=T)[1:10])
x4=as.matrix(sort(table_data_1d(ss,"start_station_name","start city","San Jose"),decreasing=T)[1:10])
x5=as.matrix(sort(table_data_1d(ss,"start_station_name","start city","Emeryville"),decreasing=T)[1:10])
origins=cbind(rownames(x1),x1,
              rownames(x2),x2,
              rownames(x3),x3,
              rownames(x4),x4,
              rownames(x5),x5)
rownames(origins)=NULL
colnames(origins)=c(rep("San Francisco",2),rep("Oakland",2),rep("Berkeley",2),rep("san Jose",2),rep("Emeryville",2))
colnames(x1)="San Francisco";colnames(x2)="Oakland";colnames(x3)="Berkeley";colnames(x4)="San Jose";colnames(x5)="Emeryville"
x1;x2;x3;x4;x5
```

```
##                                                           San Francisco
## San Francisco Ferry Building (Harry Bridges Plaza)                44925
## The Embarcadero at Sansome St                                     39630
## San Francisco Caltrain (Townsend St at 4th St)                    39483
## San Francisco Caltrain Station 2  (Townsend St at 4th St)         39013
## Market St at 10th St                                              37403
## Berry St at 4th St                                                36824
## Montgomery St BART Station (Market St at 2nd St)                  35824
## Powell St BART Station (Market St at 4th St)                      33454
## Steuart St at Market St                                           30868
## Howard St at Beale St                                             30224
```

```
##                             Oakland
## 19th Street BART Station      18611
## MacArthur BART Station        14316
## Lake Merritt BART Station     10989
## El Embarcadero at Grand Ave   10619
## Grand Ave at Perkins St       10323
## Bay Pl at Vernon St            9991
## Frank H Ogawa Plaza            9665
## 2nd Ave at E 18th St           9275
## Grand Ave at Webster St        9162
## West Oakland BART Station      8143
```

```
##                               Berkeley
## Bancroft Way at College Ave      13302
## Bancroft Way at Telegraph Ave     9546
## Downtown Berkeley BART            7826
## Ashby BART Station                7007
## Haste St at Telegraph Ave         5988
## Parker St at Fulton St            4726
## Berkeley Civic Center             4388
## Telegraph Ave at Ashby Ave        4180
## Hearst Ave at Euclid Ave          4131
## Derby St at College Ave           3521
```

```
##                                San Jose
## San Jose Diridon Station           6912
## San Fernando St at 4th St          6058
## 5th St at Virginia St              5070
## Ryland Park                        4725
## San Salvador St at 9th St          3540
## San Fernando at 7th St             3506
## Paseo De San Antonio at 2nd St     3298
## Julian St at The Alameda           3114
## San Jose City Hall                 2875
## San Pedro Square                   2774
```

```
##                           Emeryville
## 65th St at Hollis St            2336
## Horton St at 40th St            2310
## Adeline St at 40th St           2086
## Emeryville Town Hall            1624
## 59th St at Horton St            1488
## Doyle St at 59th St             1305
## Emeryville Public Market        1267
## Stanford Ave at Hollis St       1190
## 47th St at San Pablo Ave        1141
## 53rd St at Hollis St             528
```

```r
#as.data.table(origins)
```

### Popular Destinations by City

```r
ss=filter_data_1d(FGB,"end_station_name")
x1=as.matrix(sort(table_data_1d(ss,"end_station_name","end city","San Francisco"),decreasing=T)[1:10])
x2=as.matrix(sort(table_data_1d(ss,"end_station_name","end city","Oakland"),decreasing=T)[1:10])
x3=as.matrix(sort(table_data_1d(ss,"end_station_name","end city","Berkeley"),decreasing=T)[1:10])
x4=as.matrix(sort(table_data_1d(ss,"end_station_name","end city","San Jose"),decreasing=T)[1:10])
x5=as.matrix(sort(table_data_1d(ss,"end_station_name","end city","Emeryville"),decreasing=T)[1:10])
destinations=cbind(rownames(x1),x1,
              rownames(x2),x2,
              rownames(x3),x3,
              rownames(x4),x4,
              rownames(x5),x5)
rownames(destinations)=NULL
colnames(destinations)=c(rep("San Francisco",2),rep("Oakland",2),rep("Berkeley",2),rep("san Jose",2),rep("Emeryville",2))
colnames(x1)="San Francisco";colnames(x2)="Oakland";colnames(x3)="Berkeley";colnames(x4)="San Jose";colnames(x5)="Emeryville"
x1;x2;x3;x4;x5
```

```
##                                                           San Francisco
## San Francisco Ferry Building (Harry Bridges Plaza)                50993
## San Francisco Caltrain (Townsend St at 4th St)                    50806
## San Francisco Caltrain Station 2  (Townsend St at 4th St)         50007
## The Embarcadero at Sansome St                                     47048
## Montgomery St BART Station (Market St at 2nd St)                  41185
## Market St at 10th St                                              36044
## Powell St BART Station (Market St at 4th St)                      35553
## Berry St at 4th St                                                35543
## Steuart St at Market St                                           30843
## Powell St BART Station (Market St at 5th St)                      28249
```

```
##                             Oakland
## 19th Street BART Station      20053
## MacArthur BART Station        15376
## El Embarcadero at Grand Ave   11916
## Grand Ave at Perkins St       11330
## 2nd Ave at E 18th St          10120
## Frank H Ogawa Plaza            9775
## Bay Pl at Vernon St            9508
## Grand Ave at Webster St        9130
## West Oakland BART Station      8168
## Lake Merritt BART Station      7816
```

```
##                               Berkeley
## Downtown Berkeley BART           11603
## Bancroft Way at Telegraph Ave     8407
## Ashby BART Station                7579
## Berkeley Civic Center             5135
## Parker St at Fulton St            5131
## Bancroft Way at College Ave       4732
## Haste St at Telegraph Ave         4448
## Fulton St at Bancroft Way         4127
## Telegraph Ave at Ashby Ave        3993
## Addison St at Fourth St           3862
```

```
##                                San Jose
## San Jose Diridon Station           7057
## San Fernando St at 4th St          6332
## Ryland Park                        4540
## 5th St at Virginia St              4464
## San Fernando at 7th St             3992
## San Salvador St at 9th St          3744
## Paseo De San Antonio at 2nd St     3468
## Julian St at The Alameda           3119
## The Alameda at Bush St             2938
## San Pedro Square                   2763
```

```
##                           Emeryville
## 65th St at Hollis St            3562
## Horton St at 40th St            2584
## Adeline St at 40th St           2195
## 59th St at Horton St            1806
## Emeryville Town Hall            1604
## Emeryville Public Market        1579
## Stanford Ave at Hollis St       1429
## Doyle St at 59th St             1412
## 47th St at San Pablo Ave        1337
## 53rd St at Hollis St             881
```

```r
#as.data.table(destinations)
```


### Total Rides Over Time

```r
ss=filter_data_1d(FGB,"start_date",col="start_daytype")[['start_date']]
```


```r
plot(table(ss),xlab="Date",ylab="Total Rides",main="Total Rides over Time \n(All Daytypes)")
```

![](Visualizations2_files/figure-html/a0-1.png)<!-- -->


```r
plot(table(filter_data_1d(FGB,"start_date",col="start_daytype",vars=c("Saturday","Sunday"),not=T)[["start_date"]]),xlab="Date",ylab="Total Rides",main="Total Rides over Time \n(Weekdays)")
```

![](Visualizations2_files/figure-html/a-1.png)<!-- -->


```r
plot(table(filter_data_1d(FGB,"start_date",col="start_daytype",vars=c("Saturday"),not=F)[["start_date"]]),xlab="Date",ylab="Total Rides",main="Total Rides over Time \n(Saturdays)")
```

![](Visualizations2_files/figure-html/b-1.png)<!-- -->


```r
plot(table(filter_data_1d(FGB,"start_date",col="start_daytype",vars=c("Sunday"),not=F)[["start_date"]]),xlab="Date",ylab="Total Rides",main="Total Rides over Time \n(Sundays)")
```

![](Visualizations2_files/figure-html/c-1.png)<!-- -->


```r
berk=FGB[FGB[["start city"]]=="Berkeley" & FGB[["end city"]]=="Berkeley",]
ss=filter_data_1d(berk,"start_date",col="start_daytype")[['start_date']]
```

```r
plot(table(ss),xlab="Date",ylab="Total Rides",main="Total Rides over Time \n(All Daytypes) \nBerkeley")
```

![](Visualizations2_files/figure-html/berk1-1.png)<!-- -->

```r
plot(table(filter_data_1d(berk,"start_date",col="start_daytype",vars=c("Saturday"),not=F)[["start_date"]]),
     xlab="Date",ylab="Total Rides",main="Total Rides over Time \n(Saturdays) \nBerkeley")
```

![](Visualizations2_files/figure-html/berk3-1.png)<!-- -->

```r
plot(table(filter_data_1d(berk,"start_date",col="start_daytype",vars=c("Sunday"),not=F)[["start_date"]]),
     xlab="Date",ylab="Total Rides",main="Total Rides over Time \n(Sundays) \nBerkeley")
```

![](Visualizations2_files/figure-html/berk4-1.png)<!-- -->

```r
sanjose=FGB[FGB[["start city"]]=="San Jose" & FGB[["end city"]]=="San Jose",]
ss=filter_data_1d(sanjose,"start_date",col="start_daytype")[['start_date']]
```

```r
plot(table(ss),xlab="Date",ylab="Total Rides",main="Total Rides over Time \n(All Daytypes) \nSan Jose")
```

![](Visualizations2_files/figure-html/sanjose1-1.png)<!-- -->

```r
plot(table(filter_data_1d(sanjose,"start_date",col="start_daytype",vars=c("Saturday"),not=F)[["start_date"]]),
     xlab="Date",ylab="Total Rides",main="Total Rides over Time \n(Saturdays) \nSan Jose")
```

![](Visualizations2_files/figure-html/sanjose3-1.png)<!-- -->

```r
plot(table(filter_data_1d(sanjose,"start_date",col="start_daytype",vars=c("Sunday"),not=F)[["start_date"]]),
     xlab="Date",ylab="Total Rides",main="Total Rides over Time \n(Sundays) \nSan Jose")
```

![](Visualizations2_files/figure-html/sanjose4-1.png)<!-- -->

```r
sanFrancisco=FGB[FGB[["start city"]]=="San Francisco" & FGB[["end city"]]=="San Francisco",]
ss=filter_data_1d(sanFrancisco,"start_date",col="start_daytype")[['start_date']]
```

```r
plot(table(ss),xlab="Date",ylab="Total Rides",main="Total Rides over Time \n(All Daytypes) \nSan Francisco")
```

![](Visualizations2_files/figure-html/sanFrancisco1-1.png)<!-- -->

```r
plot(table(filter_data_1d(sanFrancisco,"start_date",col="start_daytype",vars=c("Saturday"),not=F)[["start_date"]]),
     xlab="Date",ylab="Total Rides",main="Total Rides over Time \n(Saturdays) \nSan Francisco")
```

![](Visualizations2_files/figure-html/sanFrancisco3-1.png)<!-- -->

```r
plot(table(filter_data_1d(sanFrancisco,"start_date",col="start_daytype",vars=c("Sunday"),not=F)[["start_date"]]),
     xlab="Date",ylab="Total Rides",main="Total Rides over Time \n(Sundays) \nSan Francisco")
```

![](Visualizations2_files/figure-html/sanFrancisco4-1.png)<!-- -->

```r
Oakland=FGB[FGB[["start city"]]=="Oakland" & FGB[["end city"]]=="Oakland",]
ss=filter_data_1d(Oakland,"start_date",col="start_daytype")[['start_date']]
```

```r
plot(table(ss),xlab="Date",ylab="Total Rides",main="Total Rides over Time \n(All Daytypes) \nOakland")
```

![](Visualizations2_files/figure-html/Oakland1-1.png)<!-- -->

```r
plot(table(filter_data_1d(Oakland,"start_date",col="start_daytype",vars=c("Saturday"),not=F)[["start_date"]]),
     xlab="Date",ylab="Total Rides",main="Total Rides over Time \n(Saturdays) \nOakland")
```

![](Visualizations2_files/figure-html/Oakland3-1.png)<!-- -->

```r
plot(table(filter_data_1d(Oakland,"start_date",col="start_daytype",vars=c("Sunday"),not=F)[["start_date"]]),
     xlab="Date",ylab="Total Rides",main="Total Rides over Time \n(Sundays) \nOakland")
```

![](Visualizations2_files/figure-html/Oakland4-1.png)<!-- -->

```r
Emeryville=FGB[FGB[["start city"]]=="Emeryville" & FGB[["end city"]]=="Emeryville",]
ss=filter_data_1d(Emeryville,"start_date",col="start_daytype")[['start_date']]
```

```r
plot(table(ss),xlab="Date",ylab="Total Rides",main="Total Rides over Time \n(All Daytypes) \nEmeryville")
```

![](Visualizations2_files/figure-html/Emeryville1-1.png)<!-- -->

```r
plot(table(filter_data_1d(Emeryville,"start_date",col="start_daytype",vars=c("Saturday"),not=F)[["start_date"]]),
     xlab="Date",ylab="Total Rides",main="Total Rides over Time \n(Saturdays) \nEmeryville")
```

![](Visualizations2_files/figure-html/Emeryville3-1.png)<!-- -->

```r
plot(table(filter_data_1d(Emeryville,"start_date",col="start_daytype",vars=c("Sunday"),not=F)[["start_date"]]),
     xlab="Date",ylab="Total Rides",main="Total Rides over Time \n(Sundays) \nEmeryville")
```

![](Visualizations2_files/figure-html/Emeryville4-1.png)<!-- -->
