####################################
#MERGING RAW DATA SETS FOR ANALYSIS#
####################################

#Merging IMOS data and trait data
imos <- read.csv("Data/Acoustic data/IMOS/ATToutput/", header=T)
#merge with life 
trait <- read.csv("Data/610.lh.data.csv", header = T)

imos$G.species <- gsub(" ", "_", imos$scientific_name) 
 
merge <- left_join(imos, lh.data, "G.species") 

library (nlme)
library(lme4)
model <- lme(log10(mcp.km) ~ log10(mass.kg) + Mean.depth, random= ~  Mean.depth | superorder, data=imos, na.action = na.exclude)
summary(model)
