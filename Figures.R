# ---
# title: "Figures"
# author: "Elly Knight"
# created: "2025-03-20"
# inputs: BRTs of model deviation
# outputs: predictor importance, marginal predictions, and interaction strength from models
# notes:

# ---

# 1. Setup ----

## 1.1 Load packages ----
library(tidyverse) # data manipulation and visualization
library(sf)

## 1.2 Set GD roots----
root <- "G:/Shared drives/ABMI_ECKnight/Projects/OSM"

## 1.3 Species guilds -----
spp <- data.frame(species = c("LEFL", "BTNW", "CMWA", "HETH", "OVEN", "PIWO", "NOWA", "CAJA", "DEJU", "RUGR", "YBSA", "BHCO", "CAWA", "OSFL"),
                  guild = c("Aerial insectivore species",
                            rep("Old-growth species", 5),
                            "Moist woodland species",
                            rep("Mixedwood/generalist species", 4),
                            "Nest parasite species",
                            rep("Species at risk", 2)))

# 2. Study area figure ----

## 2.1 Get the osm region file ----
lu <- read_sf(file.path(root, "GIS", "OSR_LU_fxd", "OSR_LUs_fxd.shp")) |> 
  st_transform(crs=3400)

## 2.2 Dissolve into one file for OSR vs not OSR ----
osr <- st_union(lu) |> 
  st_sf() |> 
  nngeo::st_remove_holes() |> 
  mutate(strata_name = "OSR")

## 2.3 Get the alberta file and take difference from ----
ab <- read_sf("G:/Shared drives/ABMI_ECKnight/GIS/CAN_adm/CAN_adm1.shp") |> 
  dplyr::filter(NAME_1=="Alberta") |> 
  st_transform(crs=3400) |> 
  st_difference(osr) |> 
  mutate(strata_name = "AB") |> 
  dplyr::select(strata_name, geometry)

## 2.4 Put them together----
abosr <- rbind(ab, osr)

## 2.5 Get the training and testing data ----
load(file.path(root, "Deviation From Expected", "Data", "Train.Rdata"))
load(file.path(root, "Deviation From Expected", "Data", "Test.Rdata"))

## 2.6 Format data for plotting ----
covs_plot <- covs_train |> 
  dplyr::select(Easting, Northing, year) |> 
  mutate(Plot = "Train") |> 
  rbind(covs_test |> 
          dplyr::select(Easting, Northing, year) |> 
          mutate(Plot = "Test")) |> 
  mutate(Plot = factor(Plot, levels = c("Train", "Test")),
         Year = as.integer(year))

## 2.6 Plot ----
plot.sa <- ggplot() +
  geom_sf(data=abosr, aes(fill=strata_name), colour="grey10", inherit.aes=FALSE) +
  geom_point(data=covs_plot, (aes(x=Easting, y=Northing, colour=Year)), alpha = 0.7) +
  scale_fill_manual(values=c("grey90", "grey60"), labels=c("Alberta", "Oil sands\nregion (OSR)"), name="") +
  scale_colour_viridis_c(name = "Year") +
  facet_wrap(~Plot) +
  scale_shape(name = "Taxa", labels=c("Birds", "Mammals", "Both")) +
  theme_bw() +
  theme(legend.position = "bottom") +
  xlab("") +
  ylab("")
plot.sa

ggsave(plot.sa, file=file.path(root,"Deviation From Expected", "Figures", "StudyArea.jpeg"), width =10, height = 8)
  
# 3. Suitability coefficients ----

## 3.1 Get the data ----
coef <- read.csv(file.path(root, "Deviation From Expected", "Results", "Coefficients.csv"))

## 3.2 Wrangle ----
coef.long <- coef |> 
  dplyr::select(-Climate) |> 
  pivot_longer(WhiteSpruceR:MineV, names_to="var", values_to="val") |> 
  mutate(val = exp(val)) |> 
  dplyr::filter(species!="CONI")

## 3.3 Summarize ----
coef.sum <- coef.long |> 
  group_by(species, var) |> 
  summarize(mn = mean(val),
            se = sd(val)/5)

## 3.4 Tidy for plotting ----
coef.tidy <- coef.sum |> 
  dplyr::filter(var %in% c("Wellsites", "EnSeismic", "EnSoftLin", "Industrial", "MineV")) |> 
  left_join(spp) |> 
  mutate(species = factor(species, levels=c("LEFL", "BTNW", "CMWA", "HETH", "OVEN", "PIWO", "NOWA", "CAJA", "DEJU", "RUGR", "YBSA", "BHCO", "CAWA", "OSFL")),
         var = factor(var, levels=c("EnSeismic", "EnSoftLin", "Wellsites", "Industrial", "MineV"),
                      labels = c("Seismic lines", "Roads", "Well pads", "Industrial", "Mine buffer")))

## 3.3 Plot ----
plot.coef <- ggplot(coef.tidy) + 
  geom_errorbar(aes(x=var, ymin = mn-1.96*se, ymax = mn+1.96*se), colour="grey30") +
  geom_point(aes(x=var, y=mn, fill=species), pch=21, alpha=0.7, size=2, colour="grey30") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.position ="bottom",
        legend.title = element_blank()) +
  facet_wrap(~guild, ncol=3, scales="free_y") +
  xlab("Oil and gas footprint type") +
  ylab("Relative abundance") +
  guides(fill=guide_legend(nrow=2))
plot.coef

ggsave(plot.coef, file=file.path(root, "Deviation From Expected", "Figures", "Suitability.jpeg"), width =10, height = 7)
  
# 4. Deviation from expected ----

## 4.1 Read in the output ----
pred <- read.csv(file.path(root, "Deviation From Expected", "Results", "Predictions.csv"))

## 4.2 Summarize ----
pred.sum <- pred |> 
  dplyr::filter(species!="CONI") |> 
  group_by(species, boot) |> 
  summarize(diff_mn = mean(residual),
            pred_mn = mean(prediction)) |> 
  ungroup() |> 
  mutate(perc_mn = diff_mn/pred_mn*100) |> 
  group_by(species) |> 
  summarize(mn = mean(perc_mn),
            se = sd(perc_mn)/5) |> 
  ungroup() |> 
  left_join(spp) |> 
  mutate(species = factor(species, levels=c("LEFL", "BTNW", "CMWA", "HETH", "OVEN", "PIWO", "NOWA", "CAJA", "DEJU", "RUGR", "YBSA", "BHCO", "CAWA", "OSFL")))

## 4.3 Plot ----
plot.dev <- ggplot(pred.sum) +
  geom_errorbar(aes(x=species, ymin = mn-1.96*se, ymax = mn+1.96*se), colour="grey30") +
  geom_point(aes(x=species, y=mn, fill=guild), pch=21, alpha=0.7, colour="grey30", size=2) +
  geom_hline(aes(yintercept=0), linetype="dashed") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust=1, vjust=1),
        axis.title.x = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank()) +
  ylab("Percent difference in density between predicted and observed") +
  ylim(c(-100, 100))
plot.dev

ggsave(plot.dev, file=file.path(root, "Deviation From Expected", "Figures", "Deviation.jpeg"), width =8, height = 6)
