# This R script is to generate a list of species and animal IDs within the TOPP Satellite dataset so that we can request
# animal metadata (sex, body size)

library(tidyverse)

sat.data <- read.csv("../../../../Movement data/3. Satellite/ATN/2019-01-13-ATN-Sat-data.csv", header = T)
head(sat.data)
unique(sat.data$commonName)
shark.sat.data <- filter(sat.data, commonName == c("Basking Shark", "Blue Shark", "Common Thresher Shark",
                                                   "Devil Ray", "Galapagos Shark", "Greenland Shark", "Grey Reef Shark", 
                                                   "Juvenile White Shark", "Manta Ray", "Oceanic Whitetip Shark", 
                                                   "Porbeagle Shark", "Salmon Shark", "Sandtiger Shark", "Shortfin Mako Shark",
                                                   "Silky Shark", "Silvertip Shark","Smooth Hammerhead", "Tiger Shark", "Whale Shark",
                                                   "White Shark"))
head(shark.sat.data)
unique(shark.sat.data$commonName)
unique(shark.sat.data$toppID)

#Generate a data table with the 6 columns to send to TOPP to get the animal metric data

TOPP.IDs <- shark.sat.data %>%
  group_by(toppID) %>% 
  summarise(TOPP.ID = first(toppID),
            serialNumber = first(serialNumber),
            commonName = first(commonName),
            yearDeployed = first(yearDeployed),
            project = first(project),
            isDrifter = first(isDrifter),
            firstDetectionTime = first(time),
            firstDetectionLong = first(longitude),
            firstDetectionLat = first(latitude),
            numberDetections = length(time))

write.csv(TOPP.IDs, file = "Data/Satellite data/TOPP.tag.summary.csv")

ross.data <- readRDS("../../../../Movement data/2. Passive/Ross - Wenlock/Wenlock-ATTdata.RDS")
unique(ross.data$Tag.Metadata$Sci.Name)
#summary
ross.summ<-ross.data$Tag.Metadata %>%
  group_by(Sci.Name) %>%
  summarise(n = length(Tag.ID))
ross.summ

#SAIAB
sa.data <- readRDS("../../../../Movement data/2. Passive/SAIAB/ATTdata_SAIAB.RDS")
sa.summ<-sa.data$Tag.Metadata %>%
  group_by(Sci.Name) %>%
  summarise(n = length(Tag.ID))
sa.summ
#summary mark-recap data
mr.data <- read.csv("../../../../Movement data/1. MarkRecap/2019-01-14_MarkRecap_data.csv", header = T)

library(ape)
data(bird.families)
tips <- bird.families$tip.label
abr <- paste("fam",1:length(tips), sep = "")
dat <- data.frame(tips, abr)
ntree <- phylotools::sub.taxa.label(bird.families, dat)
