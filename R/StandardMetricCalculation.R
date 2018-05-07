## Using ATT package to calculate standard metrics for acoustic and satellite data

#devtools::install_github("vinayudyawer/ATT")
library(ATT)
library(tidyverse)
library(lubridate)
library(data.table)
library(rgeos)
library(raster)
# library(maps); library(mapdata)

### Satellite data (ATN)
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

sharkray<-c("Basking Shark","Blue Shark","Common Thresher Shark","Galapagos Shark",
         "Greenland Shark","Grey Reef Shark", "Juvenile White Shark", "Oceanic Whitetip Shark",
         "Porbeagle Shark", "Salmon Shark", "Sandtiger Shark", "Shortfin Mako Shark", "Silky Shark",
         "Silvertip Shark", "Smooth Hammerhead", "Tiger Shark", "Whale Shark", "White Shark","Devil Ray", "Manta Ray")

srdat<-satdat %>%
  filter(commonName %in% sharkray)


# coordinates(srdat)<-c("longitude","latitude")
# projection(srdat)<-CRS("+init=epsg:4326")
# quartz(width=9, height=6); plot(srdat, cex=0.1); 
# sdat<-gDifference()

### Process data through ATT
data<-srdat

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
  group_by(serialNumber) %>%
  summarize(scientific_name = NA,
            common_name = first(commonName), 
            tag_project_name = NA, 
            release_latitude = NA, 
            release_longitude = NA, 
            ReleaseDate = NA, 
            tag_expected_life_time_days = NA, 
            tag_status = NA, 
            sex = NA,
            measurement = NA) %>%
  rename(tag_id = serialNumber)

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

ATTdata<-setupData(tagdata,taginfo,statinfo, crs=CRS("+init=epsg:4269"))

dispdat<-dispersalSummary(ATTdata)
dispdat$Velocity<-dispdat$Consecutive.Dispersal/dispdat$Time.Since.Last.Detection

dailydisp<- dispdat %>%
  filter(Consecutive.Dispersal > 0) %>%
  mutate(date=date(Date.Time)) %>%
  group_by(date, Tag.ID) %>%
  summarize(Transmitter.Name = first(Transmitter.Name),
            common_name = first(Common.Name),
            Daily.Dispersal = sum(Consecutive.Dispersal, na.rm=T),
            mean.Daily.Velocity = mean(Velocity, na.rm=T))

source("R/displot.R")
dailydisp %>% group_by(common_name) %>% summarize(n_distinct(Tag.ID))
displot(data=data.frame(dailydisp), cn="White Shark", var="Daily.Dispersal", dist="kernel")

write_csv(dispdat, path="DispersalSummary_Rays.csv")
write_csv(dailydisp, path="Daily_DispersalSummary_Rays.csv")

#####
### Activity space estimation
COAdata<- data %>% 
  transmute(Tag.ID = as.factor(serialNumber), TimeStep.coa = time, Latitude.coa = latitude, Longitude.coa = longitude, Sensor.Value.coa = NA, Sensor.Unit = NA, 
            Number.of.Stations = NA, Number.of.Detections =NA, Sci.Name =NA, Common.Name = commonName, Tag.Project = NA,
            Release.Latitude = NA, Release.Longitude = NA, Release.Date = NA, Tag.Life = NA, Tag.Status = NA, Sex = NA, Bio = NA) %>%
  structure(., class=c("tbl_df","tbl","data.frame","COA"), CRS=attr(ATTdata, "CRS"))

proj=CRS("+init=epsg:3857")
hr<-HRSummary(COAdata, projCRS=proj, type="MCP", cont=100, storepoly=T)
hr$Overall<-hr$Overall%>% dplyr::select(Tag.ID, Common.Name, MCP.100)
hr$Subsetted<-hr$Subsetted%>% dplyr::select(Tag.ID, subset, Common.Name, MCP.100)

saveRDS(hr, file="MCParea_sattelite.rds")

write_csv(hr$Overall, path="DispersalSummary_SatTag.csv")
write_csv(dailydisp, path="Daily_DispersalSummary_SatTag.csv")



###########################################
### OTN data

otndet <- as_tibble(readRDS("Data/Acoustic data/OTN/otn_aat_detections.rds"))
otnrec <- as_tibble(read.csv("Data/Acoustic data/OTN/otn_aat_receivers.csv"))
otnani <- as_tibble(read.csv("Data/Acoustic data/OTN/otn_aat_animals.csv"))
otnrel <- as_tibble(read.csv("Data/Acoustic data/OTN/otn_aat_tag_releases.csv"))

tagdata<- otndet %>%
  transmute(detection_timestamp = ymd_hms(time),
            tag_id = detection_reference_id,
            transmitter_id = transmitter_id,
            station_name = deployment_id,
            receiver_name = deployment_id, 
            latitude= latitude,
            longitude=longitude,
            sensor_value= sensor_data,
            sensor_unit = sensor_data_units)

otntag<- left_join(otnani, otnrel, by=c("animal_reference_id" = "release_reference_id"))
taginfo<- otntag %>%
  transmute(tag_id = animal_reference_id,
            scientific_name = scientificname,
            common_name = vernacularname, 
            tag_project_name = NA, 
            release_latitude = latitude, 
            release_longitude = longitude, 
            ReleaseDate = ymd_hms(time), 
            tag_expected_life_time_days = difftime(ymd_hms(time), ymd_hms(expected_enddate), "days"), 
            tag_status = NA, 
            sex = sex,
            measurement = length)

statinfo<- otnrec %>%
  transmute(station_name = deployment_id, 
            receiver_name = deployment_id, 
            installation_name = array_name, 
            project_name = deployment_project_reference, 
            deploymentdatetime_timestamp = ymd_hms(time), 
            recoverydatetime_timestamp = ymd_hms(recovery_datetime_utc), 
            station_latitude = latitude, 
            station_longitude = longitude, 
            status = NA)

attOTN<-setupData(tagdata, taginfo, statinfo, crs=CRS("+init=epsg:4269"))

detOTN<-detectionSummary(attOTN)

dispOTN<-dispersalSummary(attOTN)
dispOTN$Velocity<-dispOTN$Consecutive.Dispersal/dispOTN$Time.Since.Last.Detection

dailydispOTN<- dispOTN %>%
  filter(Consecutive.Dispersal > 0) %>%
  mutate(date=date(Date.Time)) %>%
  group_by(date, Tag.ID) %>%
  summarize(Transmitter.Name = first(Transmitter.Name),
            common_name = first(Common.Name),
            Daily.Dispersal = sum(Consecutive.Dispersal, na.rm=T),
            mean.Daily.Velocity = mean(Velocity, na.rm=T))

write_csv(dispOTN, path="DispersalSummary_OTN.csv")
write_csv(dailydispOTN, path="Daily_DispersalSummary_OTN.csv")


###################################################
## Calculate Velocity from IMOS pre-processed dataset

library(data.table)
library(lubridate)
imos<-readRDS("~/Documents/GitHub/SharkRay-Movement/Data/Acoustic data/IMOS/ATToutput/imos_dispersal.rds")

setkey(imos, tag_id)
imos$detection_timestamp<-lubridate::ymd_hms(imos$detection_timestamp)
imos<-imos[order(imos$detection_timestamp, imos$tag_id),]
imm<-imos[ , Time.Since.Last.Detection.sec := c(NA, diff(detection_timestamp)), by = tag_id]
imm$Velocity<-imm$discon/(imm$Time.Since.Last.Detection/3600)
imo<-filter(imm, discon > 0)

saveRDS(imo, file="imos_dispersal_withVelocity.rds")

