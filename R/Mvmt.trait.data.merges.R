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
trait <- dplyr::left_join(trait, ecomorpho, "G.species")

#write
write.csv(trait, file = "Data/trait.data/trait.data.610.csv")



#Merging IMOS data and fishery data for dispersal
#################################################
imos <- read.csv("Data/Acoustic data/IMOS/2018-03-16_IMOS_PassiveTelemetry.csv", header = T)
fisheries <- read.csv("Data/Fisheries data/2018-03-16_Fisheries.csv", header = T)
satellite <- read.csv("Data/Satellite data/Dispersal Summaries/DispersalSummary_SatTags.csv", header = T)

#create tag types columns
imos$tag.type <- rep("PassAcoustic", length(imos$tag_id))
fisheries$tag.type <- rep("markrecap", length(fisheries$TagID))
satellite$tag.type <- rep("Sat", length(satellite$Tag.ID))

#recalculate columns for matching units
imos <- imos %>%
  mutate(Length_cm = BodyLength_mm/10,
         body.mass.kg = mass.g/1000,
         sex = ifelse(sex=="Male", "M", "F"),
         days.at.liberty = days_det/DI)

#select columns
imos.dispersal <- dplyr::select(imos, scientific_name, Length_cm, body.mass.kg, sex, tag_id, tag_project_name, #Animal metadata 
                           tag.type, num_det, days_det, num_stat, DI, ReleaseDate, release_latitude, release_longitude, days.at.liberty,#detection/location data 
                           dis_min, dis_25, dis_50, dis_75, dis_max, dis_mean, dis_sd, #dispersal mertics
                           Habitat, Trophic.group) #additional grouping data

fisheries.dispersal <- select(fisheries, Species, Rel_FL_cm, Sex, TagID, #animal
                             tag.type, rel_date, rel_lat, rel_lon, days, #tag
                             dis) #dispersal

satellite.dispersal <- select(sat.dispersal, -Common.Name)

#rename columns
data.table::setnames(imos.dispersal, c("scientific_name","num_stat"), c("G.species", "num_stations"))

data.table::setnames(fisheries.dispersal, 
                     c("Species","Rel_FL_cm","Sex","TagID","rel_date", "rel_lat", "rel_lon","days","dis"), 
                     c("G.species","Length_cm","sex","tag_id","ReleaseDate","release_latitude","release_longitude","days.at.liberty","dis_max"))

#match columns classes
sapply(imos.dispersal, class)
sapply(fisheries.dispersal, class)
sapply(sat.dispersal, class)

imos.dispersal$sex <- as.factor(imos.dispersal$sex)
imos.dispersal$tag_id <- as.factor(imos.dispersal$tag_id)
imos.dispersal$days.at.liberty <- as.integer(imos.dispersal$days.at.liberty)
sat.dispersal$tag_id <- as.character(sat.dispersal$tag_id)
sat.dispersal$G.species <- as.character(sat.dispersal$G.species)
sat.dispersal$days.at.liberty <- as.integer(sat.dispersal$days.at.liberty)

#change date to date format
imos.dispersal$ReleaseDate <- as.character(imos.dispersal$ReleaseDate)
imos.dispersal$ReleaseDate <- as_date(imos.dispersal$ReleaseDate)
fisheries.dispersal$ReleaseDate <- as_date(fisheries.dispersal$ReleaseDate)

#trim short duration tags
imos.dispersal <- filter(imos.dispersal, num_det>0)
fisheries.dispersal <- filter(fisheries.dispersal, days.at.liberty>0)

#bind dataframes
dispersal <- bind_rows(imos.dispersal, fisheries.dispersal)
dispersal <- bind_rows(dispersal, sat.dispersal)
dispersal <- filter(dispersal, days.at.liberty>0)

dispersal$G.species <- gsub(" ", "_", dispersal$G.species) 

#Save DispersalRDS
saveRDS(dispersal, file = "Data/Dispersal.all.tagtypes.rds")

#Merging dispersal data with trait data
##################################

trait <- read.csv("Data/trait.data/trait.data.610.csv", header = T)


 
merge <- left_join(dispersal, trait, "G.species") 

write.csv(merge, file = "Data/Dispersal.Trait.csv")

plot(log10(merge$dis_max)~merge$Length_cm, col = as.factor(merge$superorder))
summary(merge$Habitat)



#Merge Activity Space Data Sets
###############################

#imos is already loaded from above

#upload sat Activity Space 

sat.as <- readRDS("Data/Satellite data/HR Summaries/MCParea_satelliteData.rds")

imos.actsp <- dplyr::select(imos, scientific_name, Length_cm, body.mass.kg, sex, tag_id, tag_project_name, #Animal metadata 
                                tag.type, num_det, days_det, num_stat, DI, ReleaseDate, release_latitude, release_longitude, days.at.liberty,#detection/location data 
                                mcp, bbk50, bbk95, dis_min, dis_25, dis_50, dis_75, dis_max, dis_mean, dis_sd, #activity space/dispersal mertics
                                Habitat, Trophic.group) #additional grouping data

data.table::setnames(imos.actsp, c("scientific_name","num_stat"), c("G.species", "num_stations"))
imos.actsp$G.species <- gsub(" ", "_", imos.actsp$G.species)

#merge with trait data
act.sp <- left_join(imos.actsp, trait, "G.species")

write.csv(act.sp, file = "Data/ActivitySpace.Trait.csv")



