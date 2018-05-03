## Using ATT package to calculate standard metrics for acoustic and satellite data

#devtools::install_github("vinayudyawer/ATT")
library(ATT)
library(tidyverse)
library(lubridate)
library(data.table)
library(maps)
library(raster)

satdat<-fread("Data/Satellite data/2018-05-03_ATN_satData.csv")

satdat <- as_tibble(satdat) %>%
  mutate(time = ymd_hms(satdat$time),
         longitude = longitude-360) ## correct longitude data to standard geographic coordinates

### Location classes include:
## 0: estimated error of >1500 m
## 1: estimated error between 500 - 1500m
## 2: estimated error of <500 m
## 3: estimated error of <250 m
## A: unbounded accuracy, only 3 messages received by satellite 
## B: unbounded accuracy, only 1 or 2 messages received by satellite
satdat<- filter(satdat, LC %in% c(2,3)) ## only retain class 2 and 3 

shark<-c("Basking Shark","Blue Shark","Common Thresher Shark","Galapagos Shark",
         "Greenland Shark","Grey Reef Shark", "Juvenile White Shark", "Oceanic Whitetip Shark",
         "Porbeagle Shark", "Salmon Shark", "Sandtiger Shark", "Shortfin Mako Shark", "Silky Shark",
         "Silvertip Shark", "Smooth Hammerhead", "Tiger Shark", "Whale Shark", "White Shark")

ray<-c("Devil Ray", "Manta Ray")

satshark<-filter(satdat, commonName %in% shark)
satray<-filter(satdat, commonName %in% ray)

# coordinates(satdat)<-c("longitude","latitude")
# projection(satdat)<-CRS("+init=epsg:4326")
# plot(satdat); map(add=T)

### Process data through ATT
n<-names(table(satshark$commonName)) #c(1,5,8,9,10,)
data<-filter(satshark, commonName %in% n[11])

tagdata<- data %>%
  transmute(detection_timestamp = time,
            tag_id = serialNumber,
            transmitter_id = toppID,
            station_name = NA,
            receiver_name = NA, 
            latitude= latitude,
            longitude=longitude,
            sensor_value= NA,
            sensor_unit = NA)

taginfo<- data %>%
  transmute(tag_id = serialNumber,
            scientific_name = NA,
            common_name = commonName, 
            tag_project_name = NA, 
            release_latitude = NA, 
            release_longitude = NA, 
            ReleaseDate = NA, 
            tag_expected_life_time_days = NA, 
            tag_status = NA, 
            sex = NA,
            measurement = NA)

statinfo<- data %>%
  transmute(station_name = NA, 
            receiver_name = NA, 
            installation_name = NA, 
            project_name = NA, 
            deploymentdatetime_timestamp = NA, 
            recoverydatetime_timestamp = NA, 
            station_latitude = NA, 
            station_longitude = NA, 
            status = NA)

ATTdata<-setupData(tagdata,taginfo,statinfo)

dispdat<-dispersalSummary(ATTdata)

dailydisp<- dispdat %>%
  mutate(date=date(Date.Time)) %>%
  group_by(date, Tag.ID) %>%
  summarize(Transmitter.Name = first(Transmitter.Name),
            common_name = first(Common.Name),
            Consecutive.Dispersal = sum(Consecutive.Dispersal, na.rm=T))

source("R/displot.R")

displot(data=data.frame(dailydisp), cn="Manta Ray", var="Consecutive.Dispersal")


### Activity space estimation
COAdata<- data %>% 
  transmute(Tag.ID = serialNumber, TimeStep.coa = time, Latitude.coa = latitude, Longitude.coa = longitude, Sensor.Value.coa = NA, Sensor.Unit = NA, 
            Number.of.Stations = NA, Number.of.Detections =NA, Sci.Name =NA, Common.Name = commonName, Tag.Project = NA,
            Release.Latitude = NA, Release.Longitude = NA, Release.Date = NA, Tag.Life = NA, Tag.Status = NA, Sex = NA, Bio = NA) %>%
  structure(., class=c("tbl_df","tbl","data.frame","COA"))

proj=CRS("+init=epsg:3410")
hr<-HRSummary(COAdata, projCRS=proj, type="MCP")



