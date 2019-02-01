#Analysis of shark and ray homerange size and dispersal

#load required packages
library(tidyverse)
library(phytools)
library(MCMCglmm)
library(diversitree)

#load data
act.sp <- read.csv("Data/ActivitySpace.Trait.csv", header = T)
#markrecap <- read.csv("Data/2018-03-16_Fisheries.csv", header = T)

act.sp <- act.sp %>%
  mutate(mcp.km = mcp/1000000)

#Setting Colour scales and themes for plotting
col.subclass <- scales::alpha(c("red", "green", "blue"), 0.5)
names(col.subclass) <- levels(act.sp$subclass)
col.tax <- scale_colour_manual(name = "subclass", values = col.subclass)

col.hab <- scales::alpha(c("red", "blue"), 0.5)
names(col.hab) <- levels(act.sp$Habitat)
col.habitat <- scale_colour_manual(name = "Habitat", values = col.hab)

col.hab.new <- scales::alpha(c("red", "yellow", "blue", "green"), 0.5)
names(col.hab.new) <- levels(act.sp.mean$Habitat)
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
HR.mod.full <- glm(log10(mcp.km)~log10(body.mass.kg) + Red.List.status.new, data = act.sp)
summary(HR.mod.full)

HR.mod.hab <- glm(log10(mcp.km)~log10(mass.kg) + habitat.new, data = imos)
summary(HR.mod.hab)


#Plot HR over mass 
AS.plot <- ggplot(subset(act.sp, !is.na(mcp.km)), aes(body.mass.kg, mcp.km)) + 
  geom_point(size=7.5, color = scales::alpha('grey50', 0.5)) +
  scale_x_log10() +
  scale_y_log10() + #labels = function(x) format(x, scientific = FALSE)) +
  annotation_logticks(colour = "grey50") +
  geom_smooth(method = lm) +
  theme_talk()
AS.plot

pdf("Plots/AS.scaling.plot.pdf", width = 10, height = 8)
AS.plot
dev.off()

#Plot HR between sharks, rays, and chimaeras
AS.tax.plot <- ggplot(subset(act.sp.mean, !is.na(mcp.km)), aes(body.mass, mcp.km, colour = subclass)) + 
  geom_point(aes(size=10)) +
  col.tax +
  scale_x_log10() +
  scale_y_log10(labels = function(x) format(x, scientific = FALSE)) +
  annotation_logticks(colour = "grey50") +
  geom_smooth(method = lm, aes(fill=subclass)) +
  theme_talk()
AS.tax.plot

pdf("Plots/AS.mean.subclass.plot.pdf", width = 10, height = 8)
AS.tax.plot
dev.off()

#Plot HR over mass and Habitat -> This is the orignal Benthic vs Pelagic classification
HR.hab.plot <- ggplot(subset(act.sp.mean, !is.na(mcp.km)), aes(body.mass, mcp.km, colour = habitat)) + 
            geom_point(aes(size=10)) +
            scale_x_log10() +
            scale_y_log10(labels = function(x) format(x, scientific = FALSE)) +
            col.habitat.new +
            annotation_logticks(colour = "grey50") +
            geom_smooth(method = lm) +
            theme_talk()
HR.hab.plot

pdf("Plots/AS.mean.habitat.plot.pdf", width = 10, height = 8)
HR.hab.plot
dev.off()

#Plot HR over mass and Habitat -> This is the new classification
HR.hab.new.plot <- ggplot(subset(act.sp, !is.na(mcp.km)), aes(body.mass.kg, mcp.km, colour = habitat)) + 
                    geom_point(aes(size=10)) +
                    scale_x_log10() +
                    scale_y_log10(labels = function(x) format(x, scientific = FALSE)) +
                    col.habitat.new +
                    annotation_logticks(colour = "grey50") +
                    geom_smooth(method = lm, aes(fill=habitat)) +
                    theme_talk()
HR.hab.new.plot

pdf("Plots/Homerange.habitat.new.plot.pdf", width = 10, height = 8)
HR.hab.new.plot
dev.off()

#Plot by ecomorphotype
AS.ecomorpho.plot <- ggplot(subset(act.sp, !is.na(mcp.km)), aes(body.mass.kg, mcp.km, colour = ecomorph.life)) + 
  geom_point(aes(size=10)) +
  scale_x_log10() +
  scale_y_log10() +
  col.ecolife +
  annotation_logticks(colour = "grey50") +
  #geom_smooth(method = lm, aes(fill=ecomorphotype)) +
  facet_wrap(~habitat2) +
  theme_talk()
AS.ecomorpho.plot

pdf("Plots/AS.ecomorpho.pdf", width = 10, height = 8)
AS.ecomorpho.plot
dev.off()

#HR plot by trophic Group...
HR.trophic.plot <- ggplot(subset(act.sp.mean, !is.na(mcp.km)), aes(body.mass, mcp.km, colour = Trophic.group)) + 
  geom_point(aes(size=10)) +
  scale_x_log10() +
  scale_y_log10(labels = function(x) format(x, scientific = FALSE)) +
  col.feed +
  annotation_logticks(colour = "grey50") +
  geom_smooth(method = lm) +
  theme_talk()
HR.trophic.plot

pdf("Plots/AS.mean.Trophic.plot.pdf", width = 10, height = 8)
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
#plot(imos$dis_max~imos$BodyLength_mm, log="xy")
#plot(imos$dis_max~imos$mass.g, log="xy")

disp <- read.csv("Data/Dispersal.Trait.csv", header = T)

col.tag <- scales::alpha(c("red", "green", "blue"), 0.5)
names(col.tag) <- levels(disp$tag.type)
color.tag <- scale_colour_manual(name = "tag.type", values = col.tag)

disp <- filter(disp, days.at.liberty>0)

Disp.plot <- ggplot(subset(disp, !is.na(dis_max)), aes(days.at.liberty, dis_max, colour = tag.type)) + 
  geom_point(aes(size=10)) +
  #scale_x_log10() +
  scale_y_log10() +
  color.tag +
  theme_talk()
Disp.plot

plot(log10(disp$dis_max)~disp$days.at.liberty, col=as.factor(disp$tag.type))

#Plot species means
act.sp.mean<-act.sp %>%
                group_by(G.species) %>%
                summarise(mcp = mean(na.exclude(mcp)),
                          bbk50 = mean(na.exclude(bbk50)),
                          bbk90 = mean(na.exclude(bbk95)),
                          days_det = mean(days_det),
                          dis_max = mean(na.exclude(dis_max)),
                          TL.mean = mean(na.omit(Length_cm)),
                          body.mass = mean(na.omit(body.mass.kg)),
                          Habitat = first(habitat),
                          range.depth = max(range.depth),
                          range.lat = max(range.lat),
                          area = max(area),
                          Trophic.group = first(Trophic.group),
                          size.at.mat = max(size.at.mat),
                          max.size = max(max.size),
                          k = max(k),
                          age.mat = max(age.mat),
                          max.age = max(max.age),
                          bearing.mode = first(bearing.mode),
                          breeding.interval = max(breeding.interval),
                          ecomorphotype = first(ecomorphotype),
                          Red.List = first(Red.List.status.new),
                          ED = max(median.ED))
                          
            
                
act.sp.mean.plot <- ggplot(subset(act.sp.mean, !is.na(mcp)), aes(body.mass,mcp, colour = Habitat)) + 
  geom_point(aes(size=10)) +
  scale_x_log10() +
  scale_y_log10() +
  col.habitat.new +
  annotation_logticks(colour = "grey50") +
  theme_talk()
act.sp.mean.plot

pdf("Plots/AS.spmean.hab.pdf", width = 10, height = 8)
act.sp.mean.plot
dev.off()


#Data Distribution plot
tree <- read.nexus("Data/trees/10cal.tree250.nex")

#data set with traits to plot
act.sp.mean.plot <- act.sp.mean





#make tree.dataset


act.mean<-act.sp %>%
  group_by(G.species) %>%
  summarise(mcp = mean(na.exclude(mcp)),
            bbk50 = mean(na.exclude(bbk50)),
            bbk90 = mean(na.exclude(bbk95)),
            days_det = max(days_det),
            dis_max = max(dis_max),
            TL.mean = mean(na.omit(Length_cm)),
            body.mass = mean(na.omit(body.mass.kg)))

trait <- read.csv("Data/trait.data/trait.data.610.csv", header = T)

plot.data <- left_join(trait, act.mean, "G.species")

plot.data <- left_join(plot.data, act.sp.mean, "G.species")
write.csv(plot.data, file = "data.distribution.csv")
plot.data <- select(plot.data, G.species, names, subclass, #superorder, order, family,
                    mcp, bbk50, bbk90, days_det,dis_max, TL.mean, body.mass, habitat,
                    range.depth, range.lat,  area, size.at.mat,  max.size, k, age.mat, max.age,
                    bearing.mode, breeding.interval, ecomorphotype, Red.List.status.new, median.ED, Trophic.group, lifestyle)
plot.data <- read.csv("data.distribution.csv", header = T)

plot.data$subclass<-ifelse(is.na(plot.data$subclass),"NoData","Data")
plot.data$max.size<-ifelse(is.na(plot.data$max.size),"NoData","Data")
#plot.data$mcp<-ifelse(is.na(plot.data$superorder),"NoData","Data")
#plot.data$mcp<-ifelse(is.na(plot.data$family),"NoData","Data")
plot.data$mcp<-ifelse(is.na(plot.data$mcp),"NoData","Data")
plot.data$bbk50<-ifelse(is.na(plot.data$bbk50),"NoData","Data")
plot.data$bbk90<-ifelse(is.na(plot.data$bbk90),"NoData","Data")
plot.data$days_det<-ifelse(is.na(plot.data$days_det),"NoData","Data")
plot.data$dis_max<-ifelse(is.na(plot.data$dis_max),"NoData","Data")
plot.data$TL.mean<-ifelse(is.na(plot.data$TL.mean),"NoData","Data")
plot.data$body.mass<-ifelse(is.na(plot.data$body.mass),"NoData","Data")
plot.data$habitat<-ifelse(is.na(plot.data$habitat),"NoData","Data")
plot.data$range.depth<-ifelse(is.na(plot.data$range.depth),"NoData","Data")
plot.data$range.lat<-ifelse(is.na(plot.data$range.lat),"NoData","Data")
plot.data$area<-ifelse(is.na(plot.data$area),"NoData","Data")
plot.data$size.at.mat<-ifelse(is.na(plot.data$size.at.mat),"NoData","Data")
plot.data$max.size<-ifelse(is.na(plot.data$max.size),"NoData","Data")
plot.data$k<-ifelse(is.na(plot.data$k),"NoData","Data")
plot.data$age.mat<-ifelse(is.na(plot.data$age.mat),"NoData","Data")
plot.data$max.age<-ifelse(is.na(plot.data$max.age),"NoData","Data")
plot.data$bearing.mode<-ifelse(is.na(plot.data$bearing.mode),"NoData","Data")
plot.data$breeding.interval<-ifelse(is.na(plot.data$breeding.interval),"NoData","Data")
plot.data$ecomorphotype<-ifelse(is.na(plot.data$ecomorphotype),"NoData","Data")
plot.data$Red.List.status.new<-ifelse(is.na(plot.data$Red.List),"NoData","Data")
plot.data$median.ED<-ifelse(is.na(plot.data$ED),"NoData","Data")
plot.data$Trophic.group<-ifelse(is.na(plot.data$Trophic.group),"NoData","Data")
plot.data$lifestyle<-ifelse(is.na(plot.data$lifestyle),"NoData","Data")

pd<-treeplyr::make.treedata(tree,plot.data, name_column = "G.species")

tree <- pd$phy
dat.dist <- pd$dat
dat.dist<-as.data.frame(dat.dist)
row.names(dat.dist) <- dat.dist$species

dat.dist$pup.dist<-as.factor(dat.dist$pup.dist)
dat.dist$mat.dist<-as.factor(dat.dist$mat.dist)

#generating colors for each trait, should just be the same for each trait NAs will just be left blank
cols <- list(subclass = c("turquoise","white"), mcp=c("turquoise","white"), bbk50=c("turquoise", "white"), bbk90=c("turquoise", "white"),
             days_det=c("turquoise", "white"), dis_max=c("turquoise", "white"),
             TL.mean=c("turquoise","white"), body.mass=c("turquoise", "white"), body.mass=c("turquoise", "white"),
             habitat=c("turquoise", "white"), range.depth=c("turquoise", "white"),
             range.lat=c("turquoise","white"), area=c("turquoise", "white"), size.at.mat=c("turquoise", "white"),
             max.size=c("turquoise", "white"), k=c("turquoise", "white"),
             age.mat=c("turquoise","white"), max.age=c("turquoise", "white"), bearing.mode=c("turquoise", "white"),
             breeding.interval=c("turquoise", "white"), ecomorphotype=c("turquoise", "white"),
             Red.List=c("turquoise","white"), ED=c("turquoise", "white"), Trophic.group = c("turquoise","white"), lifestyle=c("turquoise","white"))

cols.imcc <- list(mcp=c("red",NULL), bbk50=c("red", NULL), bbk90=c("red", NULL), dis_max=c("red", NULL),subclass = c("red",NULL),
                   Trophic.group = c("red",NULL), lifestyle=c("red",NULL), habitat=c("red",NULL), max.size=c("red",NULL))

cols.move <- list(mcp=c("red",NULL), bbk50=c("red", NULL), bbk90=c("red", NULL), dis_max=c("red", NULL),subclass = c("black",NULL),
                  Trophic.group = c("black",NULL), lifestyle=c("black",NULL), habitat=c("black",NULL), max.size=c("black",NULL))

cols.impute <- list(mcp=c("red","blue"), bbk50=c("red", "blue"), bbk90=c("red", "blue"), dis_max=c("red", "blue"),subclass = c("red",NULL),
                  Trophic.group = c("red",NULL), lifestyle=c("red",NULL), habitat=c("red",NULL), max.size=c("red",NULL))

cols.blank <- list(mcp=c("black",NULL), bbk50=c("black", NULL), bbk90=c("black", NULL), dis_max=c("black", NULL),subclass = c("black",NULL),
                  Trophic.group = c("black",NULL), lifestyle=c("black",NULL), habitat=c("black",NULL), max.size=c("black",NULL))

#plot data spread on the tree
par(bg="black", col="white")
diversitree::trait.plot(ladderize(tree, right=FALSE), dat.dist[,c("mcp", "bbk50", "bbk95", "days_det", "dis_max","TL.mean",
                                                     "body.mass", "habitat", "range.depth", "range.lat", "area",
                                                     "size.at.mat", "max.size", "k", "age.mat","max.age",
                                                     "bearing.mode","breeding.interval", "ecomorphotype",
                                                     "Red.List.status.new","median.ED")], cols, legend = F, type="p", cex.lab=0.25,w=1/20)
#for IMCC talk
trait.plot(ladderize(tree, right=FALSE),
           dat.dist[,c("mcp", "bbk50", "bbk90", "dis_max","subclass", "Trophic.group", "lifestyle", "habitat", "max.size")],
           cols.impute,
           legend = F,
           type="f",
           cex.lab=0.01,
           w=1/20,
           edge.color="white")
#trait.plot(ladderize(tree, right=FALSE), dat.dist[,c("mcp", "bbk50", "bbk90", "dis_max")], cols.move, legend = F, type="f", cex.lab=0.01,w=1/20, edge.color="white")

Cambly <- filter(act.sp, G.species == "Carcharhinus_amblyrhynchos", Length_cm>0)

hist(Cambly$Length_cm, prob = T, main="Carcharhinus amblyrhynchos", xlab = "Length (cm)", col="grey75")
abline(v=Cambly$pup.size, lty=2, col="grey50")
abline(v=Cambly$size.at.mat, col="grey50")
abline(v=Cambly$max.size, lty=2, col="grey50")
lines(density(Cambly$Length_cm), col="red")

as.filt <- filter(act.sp, count>10, G.species !="Rhincodon_typus")
ggplot(as.filt, aes(x=Length_cm)) +
  geom_histogram(binwidth=10, aes(y=..density..),colour="black",fill="grey75") + 
  geom_density(aes(y=..density..)) +
  facet_wrap(~G.species, scales = "free_y")



filter(act.sp.mean, n>10)

a <- filter(act.sp, length(G.species)>10)
