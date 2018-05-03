####################################
#MERGING RAW DATA SETS FOR ANALYSIS#
####################################


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



#Merging IMOS data and trait data
#################################

imos <- read.csv("Data/Acoustic data/IMOS/ATToutput/", header=T)
#merge with life 
trait <- read.csv("Data/610.lh.data.csv", header = T)

imos$G.species <- gsub(" ", "_", imos$scientific_name) 
 
merge <- left_join(imos, lh.data, "G.species") 

library (nlme)
library(lme4)
model <- lme(log10(mcp.km) ~ log10(mass.kg) + Mean.depth, random= ~  Mean.depth | superorder, data=imos, na.action = na.exclude)
summary(model)
