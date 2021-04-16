library(openair)
library(tidyr)
library(readr)
library(ggplot2)
library(dplyr)
library(tidyverse)

TF<- read.csv("STATION CHERAS (F)M10.csv", header = T)
TF


TF[TF == 0] <- NA 



#PARAMETERS----
PM2.5<-TF$PM2.5.1H..µg.m3.
PM10<-TF$PM10.1H..µg.m3.
SO2<-TF$SO2.CONST
NO2<-TF$NO2.PPB
NO<-TF$NO.PPB
NOX<-TF$NOX.PPB
O3<-TF$O3.PPB
CO<-TF$CO.CONST

#Met----
wd<-TF$WIND.DIRECTION.1H....
ws<-TF$WIND.SPEED.1H..m.s.
RH<-TF$RELATIVE.HUMIDITY.1H....
SolarR<-TF$SOLAR.RADIATION.1H..W.m2.
Temp<-TF$Ambient.Temperature.1H...c.

STATION<-TF$STATION.ID
LOCATION<-TF$LOCATION



#DATE----
date <- strptime(TF$'DATE.TIME', "%d/%m/%Y %H:%M") # to convert the date column into date instead of character
date <- as.POSIXct(date, tz = "Asia/Kuala_Lumpur")#change it to POSIXct and also MAKE SURE it is date not DATE or other

#MYDATA----
MyDataF<- data.frame(date,STATION,PM2.5,PM10,SO2,NO2,NO,NOX,O3,CO,wd,ws,RH,SolarR,Temp)
MyDataF
summary(MyDataF)


#PM2.5 AND PM10  CONCENTRATION---- 


# 2018-2020

timeVariation(selectByDate(MyDataF,start = "1/1/2018", end = "31/12/2020"),
              pollutant = c("PM2.5","PM10"),
              key.columns = 4,main=("PM 2.5 AND PM 10 CONCENTRATION 2018-2020"),
              col = "Set2", ci = FALSE, lwd = 3)


#2018-2019
timeVariation(selectByDate(MyDataF,start = "1/1/2018", end = "31/12/2019"),
              pollutant = c("PM2.5","PM10"),
              key.columns = 4,main="PM 2.5 AND PM 10 CONCENTRATION 2018-2019",
              col = "Set2", ci = FALSE, lwd = 3)

#2018
timeVariation(selectByDate(MyDataF,start = "1/1/2018", end = "31/12/2018"),
              pollutant = c("PM2.5","PM10"),
              key.columns = 4,main="PM 2.5 AND PM 10 CONCENTRATION 2018",
              col = "Set2", ci = FALSE, lwd = 3)

#2019
timeVariation(selectByDate(MyDataF,start = "1/1/2019", end = "31/12/2019"),
              pollutant = c("PM2.5","PM10"),
              key.columns = 4,main="PM 2.5 AND PM 10 CONCENTRATION 2019",
              col = "Set2", ci = FALSE, lwd = 3)

#sEPT 2019
timeVariation(selectByDate(MyDataF,start = "1/1/2019", end = "30/9/2019"),
              pollutant = c("PM2.5","PM10"),
              key.columns = 4,main="PM 2.5 AND PM 10 CONCENTRATION SEPT 2019",
              col = "Set2", ci = FALSE, lwd = 3)

#2020
timeVariation(selectByDate(MyDataF,start = "1/1/2020", end = "31/12/20"),
              pollutant = c("PM2.5","PM10"),
              key.columns = 4,main="PM 2.5 AND PM 10 CONCENTRATION 2020",
              col = "Set2", ci = FALSE, lwd = 3)




# 2018-2020

timeVariation(selectByDate(MyDataF,start = "1/1/2018", end = "31/12/2020"),
              pollutant = c("PM2.5","PM10","SO2","NO2","NO","NOX","O3","CO"),
              key.columns = 4,main="2018-2020",
              col = "Set2", ci = FALSE, lwd = 3)


#2018-2019
timeVariation(selectByDate(MyDataF,start = "1/1/2018", end = "31/12/2019"),
              pollutant = c("PM2.5","PM10","SO2","NO2","NO","NOX","O3","CO"),
              key.columns = 4,main="2018-2019",
              col = "Set2", ci = FALSE, lwd = 3)

#2018
timeVariation(selectByDate(MyDataF,start = "1/1/2018", end = "31/12/2018"),
              pollutant = c("PM2.5","PM10","SO2","NO2","NO","NOX","O3","CO"),
              key.columns = 4,main="2018",
              col = "Set2", ci = FALSE, lwd = 3)

#2019
timeVariation(selectByDate(MyDataF,start = "1/1/2019", end = "31/12/2019"),
              pollutant = c("PM2.5","PM10","SO2","NO2","NO","NOX","O3","CO"),
              key.columns = 4,main="2019",
              col = "Set2", ci = FALSE, lwd = 3)

#2020
timeVariation(selectByDate(MyDataF,start = "1/1/2020", end = "31/12/20"),
              pollutant = c("PM2.5","PM10","SO2","NO2","NO","NOX","O3","CO"),
              key.columns = 4,main="2020",
              col = "Set2", ci = FALSE, lwd = 3)

#TIME VARIATION RATIO PM2.5/PM10----

#RATIO
mydataFr <- mutate(MyDataF, ratio = PM2.5/PM10)
mydataFr
summary(mydataFr$ratio)


# 2018-2020

timeVariation(selectByDate(mydataFr,start = "1/1/2018", end = "31/12/2020"),
              pollutant ="ratio",
              key.columns = 4,main="2018-2020 PM2.5/PM10 RATIO",
              col = "Set2", ci = FALSE, lwd = 3)


#2018-2019
timeVariation(selectByDate(mydataFr,start = "1/1/2018", end = "31/12/2019"),
              pollutant ="ratio",
              key.columns = 4,main="2018-2019 PM2.5/PM10 RATIO",
              col = "Set2", ci = FALSE, lwd = 3)

#2018
timeVariation(selectByDate(mydataFr,start = "1/1/2018", end = "31/12/2018"),
              pollutant ="ratio",
              key.columns = 4,main="2018 PM2.5/PM10 RATIO",
              col = "Set2", ci = FALSE, lwd = 3)

#2019
timeVariation(selectByDate(mydataFr,start = "1/1/2019", end = "31/12/2019"),
              pollutant ="ratio",
              key.columns = 4,main="2019 PM2.5/PM10 RATIO",
              col = "Set2", ci = FALSE, lwd = 3)

#2020
timeVariation(selectByDate(mydataFr,start = "1/1/2020", end = "31/12/20"),
              pollutant ="ratio",
              key.columns = 4,main="2020 PM2.5/PM10 RATIO",
              col = "Set2", ci = FALSE, lwd = 3)


#Ratio polar plot----

mydataFr <- mutate(MyDataF, ratio = PM2.5/PM10)
mydataFr
summary(mydataFr$ratio)




#2018-2020

polarPlot(selectByDate(mydataFr , year = c(2018, 2019,2020)), 
          pollutant = "ratio", col = "jet", 
          key.position = "bottom",
          key.header = "PM2.5/PM10 2018-2020", 
          key.footer = NULL)

polarPlot(selectByDate(mydataFr , year = c(2018, 2019)), 
          pollutant = "ratio", col = "jet", 
          key.position = "bottom",
          key.header = "PM2.5/PM10 2018-2019", 
          key.footer = NULL)

polarPlot(selectByDate(mydataFr, year = 2018), 
          pollutant = "ratio", col = "jet", 
          key.position = "bottom",
          key.header = " PM2.5/PM10 2018", 
          key.footer = NULL)

polarPlot(selectByDate(mydataFr , year = 2019), 
          pollutant = "ratio", col = "jet", 
          key.position = "bottom",
          key.header = "PM2.5/PM10 2019", 
          key.footer = NULL)

polarPlot(selectByDate(mydataFr, year = 2020), 
          pollutant = "ratio", col = "jet", 
          key.position = "bottom",
          key.header = "PM2.5/PM10 2020", 
          key.footer = NULL)


polarPlot(selectByDate(mydataFr, year = 2019,month=9), 
          pollutant = "ratio", col = "jet", 
          key.position = "bottom",
          key.header = "PM2.5/PM10 SEPT 2019", 
          key.footer = NULL)

