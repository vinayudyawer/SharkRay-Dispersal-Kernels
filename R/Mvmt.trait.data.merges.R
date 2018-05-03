#merge datasets for analysis

#imos data is already loaded
#merge with life 
 lh.data <- read.csv("Data/610.lh.data.csv", header = T)

imos$G.species <- gsub(" ", "_", imos$scientific_name) 
 
merge <- left_join(imos, lh.data, "G.species") 

library (nlme)
library(lme4)
model <- lme(log10(mcp.km) ~ log10(mass.kg) + Mean.depth, random= ~  Mean.depth | superorder, data=imos, na.action = na.exclude)
summary(model)
