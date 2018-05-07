satellite <- read.csv("Data/Satellite data/Dispersal Summaries/DispersalSummary_SatTags.csv", header = T)
satellite <- splitstackshape::cSplit(satellite, 'Date.Time',sep = "T", type.convert = F)
satellite$date <- lubridate::ymd(satellite$Date.Time_1)

sat.dispersal <- satellite %>%
  group_by(Tag.ID) %>%
  summarise(tag.type = first(tag.type),
            Common.Name = first(Common.Name),
            days.at.liberty = last(date) - first(date),
            dis_min = min(Consecutive.Dispersal, na.rm = T),
            dis_25 = quantile(Consecutive.Dispersal, probs=0.25, na.rm = T),
            dis_50 = quantile(Consecutive.Dispersal, probs=0.5, na.rm = T),
            dis_75 = quantile(Consecutive.Dispersal, probs=0.75, na.rm = T),
            dis_max = max(Consecutive.Dispersal, na.rm = T),
            dis_mean = mean(Consecutive.Dispersal, na.rm = T),
            dis_sd = sd(Consecutive.Dispersal, na.rm = T),
            vel_min = min(Velocity, na.rm = T),
            vel_25 = quantile(Velocity, probs=0.25, na.rm = T),
            vel_50 = quantile(Velocity, probs=0.5, na.rm = T),
            vel_75 = quantile(Velocity, probs=0.75, na.rm = T),
            vel_max = max(Velocity, na.rm = T),
            vel_mean = mean(Velocity, na.rm = T),
            vel_sd = sd(Velocity, na.rm = T)) 
             
sat.dispersal <- filter(sat.dispersal, !is.na(dis_25))


sat.dispersal <- sat.dispersal %>%
  mutate(G.species = forcats::fct_recode(Common.Name, Carcharodon_carcharias= "White Shark", 
                                Prionace_glauca= "Blue Shark",
                                Cetorhinus_maximus= "Basking Shark",
                                Alopias_vulpinus= "Common Thresher Shark",
                                Mobual_spp.= "Devil Ray",
                                Carcharhinus_galapagensis= "Galapagos Shark",
                                Somniosus_microcephalus= "Greenland Shark",
                                Carcharhinus_amblyrhynchos= "Grey Reef Shark",
                                Carcharodon_carcharias= "Juvenile White Shark",
                                Manta_birostris= "Manta Ray",
                                Carcharhinus_longimanus= "Oceanic Whitetip Shark",
                                Lamna_nasus= "Porbeagle Shark",
                                Lamna_ditropis= "Salmon Shark",
                                Carcharias_taurus= "Sandtiger Shark",
                                Isurus_oxyrinchus="Shortfin Mako Shark",
                                Carcharhinus_falciformis="Silky Shark",
                                Carcharhinus_albimarginatus= "Silvertip Shark",
                                Galeocerdo_cuvier= "Tiger Shark",
                                Rhincodon_typus= "Whale Shark"))

data.table::setnames(sat.dispersal, "Tag.ID", "tag_id")     

sat.dispersal <- splitstackshape::cSplit(sat.dispersal, 'days.at.liberty',sep = " ", type.convert = F)
sat.dispersal <- rename(sat.dispersal, days.at.liberty = days.at.liberty_1)

