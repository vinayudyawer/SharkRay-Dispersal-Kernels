####################################
#MERGING RAW DATA SETS FOR ANALYSIS#
####################################

library(magrittr)
library(dplyr)
library(lubridate)

#Merging trait data sets
########################

#1. Merge latitude and ED data by IUCN-name (lat data is only in IUCN name, but ED has correct names)
lat <- read.csv("Data/trait.data/lat.csv", header = T)
ed <- read.csv("Data/trait.data/ED.score.csv", header = T)

ed.lat <- dplyr::left_join(ed, lat, "IUCN_Scientific_name")
ed.lat$G.species <- ed.lat$EDGE_Scientific_name

#2. Merge ed.lat data with 610 species trait data
trait.610 <- read.csv("Data/trait.data/610sp.trait.data.csv", header = T)

trait <- dplyr::left_join(trait.610, ed.lat, "G.species")
trait <- dplyr::select(trait, G.species, subclass, superorder, order, family, Genus, species, IUCN_Scientific_name, #taxonomy
                       size.at.mat, max.size, pup.size, length.weight.a, length.weight.b, #size.traits
                       habitat, habitat2, mindepth_m, maxdepth_m, mediandepth_m, depthrange_m, mi.lat, max.lat, lat.range, area.x, #habitat/range
                       age.mat, max.age, k, #age/growth
                       bearing.mode, trophic.mode, repro.coarse, mode, interval, development, litter.size, #Reproductive traits
                       median, Red.List.status.new) #ED and threat status

#write
write.csv(trait, file = "Data/trait.data/trait.data.610.csv")



#Merging IMOS data and fishery data for dispersal
#################################################
imos <- read.csv("Data/Acoustic data/IMOS/2018-03-16_IMOS_PassiveTelemetry.csv", header = T)
fisheries <- read.csv("Data/Fisheries data/2018-03-16_Fisheries.csv", header = T)

#create tag types columns
imos$tag.type <- rep("PassAcoustic", length(imos$tag_id))
fisheries$tag.type <- rep("markrecap", length(fisheries$TagID))

#recalculate columns for matching units
imos.dispersal <- imos %>%
  mutate(Length_cm = BodyLength_mm/10,
         body.mass.kg = mass.g/1000,
         sex = ifelse(sex=="Male", "M", "F"),
         days.at.liberty = days_det/DI)

#select columns
imos.dispersal <- dplyr::select(imos.dispersal, scientific_name, Length_cm, body.mass.kg, sex, tag_id, tag_project_name, #Animal metadata 
                           tag.type, num_det, days_det, num_stat, DI, ReleaseDate, release_latitude, release_longitude, days.at.liberty,#detection/location data 
                           dis_min, dis_25, dis_50, dis_75, dis_max, dis_mean, dis_sd, #dispersal mertics
                           Habitat, Trophic.group) #additional grouping data

fisheries.dispersal <- select(fisheries, Species, Rel_FL_cm, Sex, TagID, #animal
                             tag.type, rel_date, rel_lat, rel_lon, days, #tag
                             dis) #dispersal

#rename columns
data.table::setnames(imos.dispersal, c("scientific_name","num_stat"), c("G.species", "num_stations"))
data.table::setnames(fisheries.dispersal, 
                     c("Species","Rel_FL_cm","Sex","TagID","rel_date", "rel_lat", "rel_lon","days","dis"), 
                     c("G.species","Length_cm","sex","tag_id","ReleaseDate","release_latitude","release_longitude","days.at.liberty","dis_max"))

#match columns classes
sapply(imos.dispersal, class)
sapply(fisheries.dispersal, class)

imos.dispersal$sex <- as.factor(imos.dispersal$sex)
imos.dispersal$tag_id <- as.factor(imos.dispersal$tag_id)
imos.dispersal$days.at.liberty <- as.integer(imos.dispersal$days.at.liberty)

imos.dispersal$ReleaseDate <- as_date(imos.dispersal$ReleaseDate)
fisheries.dispersal$ReleaseDate <- as_date(fisheries.dispersal$ReleaseDate)


#bind dataframes
dispersal <- bind_rows(imos.dispersal, fisheries.dispersal)


#Merging dispersal data with trait data
##################################

trait <- read.csv("Data/610.lh.data.csv", header = T)

imos$G.species <- gsub(" ", "_", imos$scientific_name) 
 
merge <- left_join(imos, lh.data, "G.species") 

library (nlme)
library(lme4)
model <- lme(log10(mcp.km) ~ log10(mass.kg) + Mean.depth, random= ~  Mean.depth | superorder, data=imos, na.action = na.exclude)
summary(model)
