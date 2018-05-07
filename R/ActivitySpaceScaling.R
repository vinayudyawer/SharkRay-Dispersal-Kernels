#Analysis of shark and ray homerange size and dispersal

#load required packages
library(tidyverse)
library(phytools)
library(MCMCglmm)

#load data
act.sp <- read.csv("Data/ActivitySpace.Trait.csv", header = T)
#markrecap <- read.csv("Data/2018-03-16_Fisheries.csv", header = T)


#Setting Colour scales and themes for plotting
col.subclass <- scales::alpha(c("red", "green", "blue"), 0.5)
names(col.subclass) <- levels(act.sp$subclass)
col.tax <- scale_colour_manual(name = "subclass", values = col.subclass)

col.hab <- scales::alpha(c("red", "blue"), 0.5)
names(col.hab) <- levels(act.sp$Habitat)
col.habitat <- scale_colour_manual(name = "Habitat", values = col.hab)

col.hab.new <- scales::alpha(c("red", "yellow", "blue", "green"), 0.5)
names(col.hab.new) <- levels(act.sp$habitat.new)
col.habitat.new <- scale_colour_manual(name = "habitat.new", values = col.hab.new)

col.trophic <- scales::alpha(c("blue", "royalblue", "skyblue", "navy"), 0.5)
names(col.trophic) <- levels(act.sp$Trophic.group)
col.feed <- scale_colour_manual(name= "Trophic.group", values = col.trophic)

#col.ecomorph

#loading ggplot2 talk theme (transparent bkg with grey/white)
source("R/Talk_theme_ggplot.R")

#plot(imos$bbk95~imos$mass.g, log="xy",
#    pch=21, bg=col.hab)
#
#plot(imos$bbk95~imos$mass.g, log="xy",
#     pch=21, bg=as.factor(imos$Trophic.group))
#
#plot(imos$bbk95~imos$mass.g, log="xy",
#     pch=21, bg=as.factor(imos$sex))

#model HR size
HR.mod.full <- glm(log10(mcp.km)~log10(mass.kg) + Habitat + Trophic.group, data = imos)
summary(HR.mod.full)

HR.mod.hab <- glm(log10(mcp.km)~log10(mass.kg) + habitat.new, data = imos)
summary(HR.mod.hab)


#Plot HR over mass 
HR.plot <- ggplot(subset(imos, !is.na(mcp.km)), aes(mass.kg, mcp.km)) + 
  geom_point(size=7.5, color = scales::alpha('grey50', 0.5)) +
  scale_x_log10() +
  scale_y_log10() +
  annotation_logticks(colour = "grey50") +
  geom_smooth(method = lm) +
  theme_talk()
HR.plot

pdf("Plots/Homerange.plot.pdf", width = 10, height = 8)
HR.plot
dev.off()

#Plot HR between sharks, rays, and chimaeras
HR.tax.plot <- ggplot(subset(imos, !is.na(mcp.km)), aes(mass.kg, mcp.km, colour = tribe)) + 
  geom_point(aes(size=10)) +
  scale_x_log10() +
  scale_y_log10() +
  col.tax +
  annotation_logticks(colour = "grey50") +
  geom_smooth(method = lm, aes(fill=tribe)) +
  theme_talk()
HR.tax.plot

pdf("Plots/Homerange.tax.plot.pdf", width = 10, height = 8)
HR.tax.plot
dev.off()

#Plot HR over mass and Habitat -> This is the orignal Benthic vs Pelagic classification
HR.hab.plot <- ggplot(subset(imos, !is.na(mcp.km)), aes(mass.kg, mcp.km, colour = Habitat)) + 
            geom_point(aes(size=10)) +
            scale_x_log10() +
            scale_y_log10() +
            col.habitat +
            annotation_logticks(colour = "grey50") +
            geom_smooth(method = lm, aes(fill=Habitat)) +
            theme_talk()
HR.hab.plot

pdf("Plots/Homerange.habitat.plot.pdf", width = 10, height = 8)
HR.hab.plot
dev.off()

#Plot HR over mass and Habitat -> This is the new classification
HR.hab.new.plot <- ggplot(subset(imos, !is.na(mcp.km)), aes(mass.kg, mcp.km, colour = habitat.new)) + 
                    geom_point(aes(size=10)) +
                    scale_x_log10() +
                    scale_y_log10() +
                    col.habitat.new +
                    annotation_logticks(colour = "grey50") +
                    geom_smooth(method = lm, aes(fill=habitat.new)) +
                    theme_talk()
HR.hab.new.plot

pdf("Plots/Homerange.habitat.new.plot.pdf", width = 10, height = 8)
HR.hab.new.plot
dev.off()


#HR plot by trophic Group...
HR.trophic.plot <- ggplot(subset(imos, !is.na(mcp.km)), aes(mass.kg, mcp.km, colour = Trophic.group)) + 
  geom_point(aes(size=10)) +
  scale_x_log10() +
  scale_y_log10() +
  col.feed +
  annotation_logticks(colour = "grey50") +
  geom_smooth(method = lm) +
  theme_talk()
HR.trophic.plot

pdf("Plots/Homerange.Trophic.plot.pdf", width = 10, height = 8)
HR.trophic.plot
dev.off()

HR.hab.plot <- ggplot(subset(imos, !is.na(mcp)), aes(mass.kg, mcp, colour = Habitat)) + 
  geom_point(aes(size=10)) +
  scale_x_log10() +
  scale_y_log10() +
  col.habitat +
  annotation_logticks(colour = "grey50") +
  theme_talk()
HR.hab.plot

#plot dispersal
plot(imos$dis_max~imos$BodyLength_mm, log="xy")
plot(imos$dis_max~imos$mass.g, log="xy")

Disp.plot <- ggplot(subset(imos, !is.na(dis_max)), aes(mass.g, dis_max, colour = Habitat)) + 
  geom_point(aes(size=10)) +
  scale_x_log10() +
  scale_y_log10() +
  col.habitat +
  theme_talk()
Disp.plot

#Plot species means
imos.mean<-imos %>%
                group_by(scientific_name) %>%
                summarise(bbk.95.mean = mean(na.omit(bbk95.km)),
                          bbk.max = max(bbk95.km),
                          bbk.min = min(bbk95.km),
                          mass.mean = mean(na.omit(mass.kg)),
                          TL.mean = mean(na.omit(BodyLength_mm)),
                          Habitat = first(Habitat))
                
ggplot(subset(imos.mean, !is.na(bbk.95.mean)), aes(TL.mean,bbk.95.mean, colour = Habitat)) + 
  geom_point(aes(size=10)) +
  scale_x_log10() +
  scale_y_log10() +
  col.habitat +
  annotation_logticks(colour = "grey50") +
  theme_talk()
