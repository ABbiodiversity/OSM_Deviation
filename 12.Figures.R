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
library(paletteer)

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

## 1.4 Guild colours ----
guilds1 <- paletteer_d("ggthemes::excel_Feathered")
guilds <- guilds1[c(1,2,5,4,3,6)]
#Yeah that's right it's a bird palette, obviously

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

covs_train_mam <- read_sf(file.path(root, "Deviation From Expected", "Data", "Mammallocs", "Site locations used for mammal deviation modeling.shp"))

covs_test_mam <- read_sf(file.path(root, "Deviation From Expected", "Data", "Mammallocs", "Site locations used for mammal forecasting.shp"))

## 2.6 Format data for plotting ----
covs_plot <- covs_train |> 
  dplyr::select(Easting, Northing, year) |> 
  mutate(Plot = "Train") |> 
  rbind(covs_test |> 
          dplyr::select(Easting, Northing, year) |> 
          mutate(Plot = ifelse(year < 2020, "Test", "Forecast"))) |> 
  mutate(Plot = factor(Plot, levels = c("Train", "Test", "Forecast")),
         Year = as.integer(year),
         Taxa = "Birds") |> 
  dplyr::select(-year) |> 
  rbind(covs_test_mam |> 
          st_transform(crs=3400) |> 
          st_coordinates() |> 
          data.frame() |> 
          mutate(Plot = "Forecast",
                 Taxa = "Mammals",
                 Year = covs_test_mam$year) |> 
          rename(Easting = X, Northing = Y),
        covs_train_mam |> 
          st_transform(crs=3400) |> 
          st_coordinates() |> 
          data.frame() |> 
          mutate(Plot = "Train",
                 Taxa = "Mammals",
                 Year = covs_train_mam$year) |> 
          rename(Easting = X, Northing = Y)) |> 
  mutate(Year = as.numeric(Year)) |> 
  dplyr::filter(Northing < st_bbox(osr)$ymax)

## 2.6 Plot ----
plot.sa <- ggplot() +
  geom_sf(data=abosr, aes(fill=strata_name), colour="grey10", inherit.aes=FALSE) +
  geom_point(data=covs_plot, (aes(x=Easting, y=Northing, colour=Year)), alpha = 0.7) +
  scale_fill_manual(values=c("grey90", "grey60"), labels=c("Alberta", "Oil sands\nregion (OSR)"), name="") +
  scale_colour_viridis_c(name = "Year") +
  facet_grid(Taxa~Plot, drop=TRUE) +
  scale_shape(name = "Taxa", labels=c("Birds", "Mammals", "Both")) +
  theme_bw() +
  theme(legend.position = "bottom") +
  xlab("") +
  ylab("")
plot.sa

ggsave(plot.sa, file=file.path(root,"Deviation From Expected", "Figures", "StudyArea.jpeg"), width =8, height = 10)
  
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

## 3.4 Get predictions ----
pred <- read.csv(file.path(root, "Deviation From Expected", "Results", "Predictions.csv"))

## 3.5 Summarize ----
pred.sum <- pred |> 
  group_by(species, boot) |> 
  summarize(pred = mean(prediction)) |> 
  group_by(species) |> 
  summarize(mn = mean(pred),
            se = sd(pred)/5) |> 
  ungroup() |> 
  mutate(var = "Other") |> 
  dplyr::filter(species!="CONI") |> 
  mutate(species = factor(species, levels=c("LEFL", "BTNW", "CMWA", "HETH", "OVEN", "PIWO", "NOWA", "CAJA", "DEJU", "RUGR", "YBSA", "BHCO", "CAWA", "OSFL")))

## 3.6 Tidy & put together for plotting ----
coef.tidy <- coef.sum |> 
  dplyr::filter(var %in% c("Wellsites", "EnSeismic", "EnSoftLin", "Industrial", "MineV", "Other"))  |> 
  left_join(spp) |> 
  mutate(species = factor(species, levels=c("LEFL", "BTNW", "CMWA", "HETH", "OVEN", "PIWO", "NOWA", "CAJA", "DEJU", "RUGR", "YBSA", "BHCO", "CAWA", "OSFL")),
         var = factor(var, levels=c("Other", "EnSeismic", "EnSoftLin", "Wellsites", "Industrial", "MineV"),
                      labels = c("Other", "Seismic lines", "Roads", "Well pads", "Industrial", "Mine buffer")))

## 3.7 Plot ----
plot.coef <- ggplot(coef.tidy) + 
  geom_rect(data=pred.sum, aes(xmin = -Inf, xmax = Inf, ymin=mn-1.96*se, ymax=mn+1.96*se), alpha = 0.2, fill="grey70") +
  geom_hline(data=pred.sum, aes(yintercept = mn), linetype="dashed", colour="grey70") +
  geom_errorbar(aes(x=var, ymin = mn-1.96*se, ymax = mn+1.96*se, colour=guild), width=0.25) +
  geom_point(aes(x=var, y=mn, fill=guild, colour=guild), pch=21, size=2.75) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        legend.position ="bottom",
        legend.title = element_blank()) +
  scale_colour_manual(values=guilds) +
  scale_fill_manual(values=guilds) +
  facet_wrap(~species, ncol=3, scales="free_y") +
  xlab("Oil and gas footprint type") +
  ylab("Density (birds/ha)") +
  guides(fill=guide_legend(nrow=2))
plot.coef

ggsave(plot.coef, file=file.path(root, "Deviation From Expected", "Figures", "Suitability.jpeg"), width = 8, height = 10)
  
# 4. Deviation from expected ----

## 4.1 Read in the output ----
pred <- read.csv(file.path(root, "Deviation From Expected", "Results", "Predictions.csv"))

## 4.2 Summarize ----
pred.sum <- pred |> 
  dplyr::filter(species!="CONI") |> 
  group_by(dataset, species, boot) |> 
  summarize(diff_mn = mean(residual),
            pred_mn = mean(prediction)) |> 
  ungroup() |> 
  mutate(perc_mn = diff_mn/pred_mn*100) |> 
  group_by(dataset, species) |> 
  summarize(mn = mean(perc_mn),
            se = sd(perc_mn)/5) |> 
  ungroup() |> 
  left_join(spp) |> 
  mutate(species = factor(species, levels=c("LEFL", "BTNW", "CMWA", "HETH", "OVEN", "PIWO", "NOWA", "CAJA", "DEJU", "RUGR", "YBSA", "BHCO", "CAWA", "OSFL")),
         dataset = factor(dataset, levels=c("test", "forecast"), labels=c("Test", "Forecast")))

## 4.3 Plot ----
plot.dev <- ggplot(pred.sum) +
  geom_errorbar(aes(x=species, ymin = mn-1.96*se, ymax = mn+1.96*se, group=dataset, alpha=dataset, colour=guild), width=0.25) +
  geom_point(aes(x=species, y=mn, group=dataset, alpha=dataset, colour=guild), size=2.75) +
  geom_hline(aes(yintercept=0), linetype="dashed") +
  scale_fill_manual(values=guilds, name="") +
  scale_colour_manual(values=guilds, name="") +
  scale_alpha_manual(values=c(0.4, 1), name="Dataset") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust=1, vjust=1),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=14),
        legend.position = "bottom") +
  ylab("Percent difference in density\nbetween predicted and observed")
plot.dev

ggsave(plot.dev, file=file.path(root, "Deviation From Expected", "Figures", "Deviation.jpeg"), width =9, height = 6)

# 5. Drivers of deviation ----

## 5.1 Load data ----
load(file.path(root, "Deviation From Expected", "Results", "ModelsOfDeviation.Rdata"))

## 5.2 Some wrangling----
out_plot <- out_raw |> 
  dplyr::filter(var!="(Intercept)") |> 
  left_join(spp) |> 
  mutate(species = factor(species, levels=c("LEFL", "BTNW", "CMWA", "HETH", "OVEN", "PIWO", "NOWA", "CAJA", "DEJU", "RUGR", "YBSA", "BHCO", "CAWA", "OSFL")),
         scale = ifelse(var %in% c("proproad", "propseismi", "propmine", "cei", "propallwel", "proppipe"), "Landscape", "Local"),
         var = factor(var, levels=c("badr_linear", "badr_lowwells", "badr_roads", "badr_highwells", "badr_minebuffer", "propseismi", "proproad", "propallwel", "proppipe", "propmine", "cei"),
                      labels = c("Local - dense linear features", "Local - low activity well pads", "Local - roads", "Local - high activity well pads", "Local - plant/mine buffer", "Landscape - seismic lines", "Landscape - roads", "Landscape - well pads", "Landscape - pipelines", "Landscape - plant/mine", "Landscape - cumulative footprint index")),
         Interpretation = ifelse(p < 0.01, "Significant", "Nonsigificant"))

## 5.3 Plot ----
plot.effects <- ggplot(out_plot) + 
  geom_errorbar(aes(x=species, ymin = estimate-1.96*se, ymax = estimate+1.96*se, colour=guild, alpha=Interpretation),
                width=0.25) +
  geom_point(aes(x=species, y=estimate, colour=guild, alpha = Interpretation),
             size=2.75) +
  geom_hline(aes(yintercept=0), linetype="dashed") +
  facet_wrap(~var, scales="free_y", ncol=3) +
  scale_colour_manual(values=guilds, name="") +
  scale_fill_manual(values=guilds, name="") +
  scale_alpha_manual(values=c(0.3, 1))+
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust=1, vjust=1),
        axis.title.x = element_blank(),
        legend.position = "bottom") +
  ylab("Effect on deviation from expected (birds/ha)")
plot.effects

ggsave(plot.effects, file=file.path(root, "Deviation From Expected", "Figures", "Deviation_Effects.jpeg"), width=9, height = 10)

# X. Individual suitability plots ----

## X.1 Get data ----
coef <- read.csv(file.path(root, "Deviation From Expected", "Results", "Coefficients.csv"))

## X.2 Wrangle ----
coef.long <- coef |> 
  dplyr::select(-Climate) |> 
  rowwise() |> 
  mutate(TreedFen = mean(TreedFenR, TreedFen1, TreedFen2, TreedFen3, TreedFen4, TreedFen5, TreedFen6, TreedFen7, TreedFen8)) |> 
  ungroup() |> 
  pivot_longer(c(WhiteSpruceR:MineV, TreedFen), names_to="var", values_to="val") |> 
  mutate(val = exp(val)) |> 
  dplyr::filter(species!="CONI")

## X.3 Summarize ----
coef.sum <- coef.long |> 
  group_by(species, var) |> 
  summarize(mn = mean(val),
            se = sd(val)/5) |> 
  left_join(spp) |> 
  dplyr::filter(str_sub(var, 1, 2)!="CC",
                !var %in% c("HardLin", "Water", "Bare", "SnowIce", "Mine", "Bare"),
                !var %in% c("TreedFenR", "TreedFen1",  "TreedFen2", "TreedFen3", "TreedFen4", "TreedFen5", "TreedFen6", "TreedFen7",  "TreedFen8")) |> 
  mutate(var = factor(var, levels=c("WhiteSpruceR", "WhiteSpruce1", "WhiteSpruce2", "WhiteSpruce3", "WhiteSpruce4", "WhiteSpruce5", "WhiteSpruce6", "WhiteSpruce7", "WhiteSpruce8", "PineR", "Pine1", "Pine2", "Pine3", "Pine4", "Pine5", "Pine6", "Pine7", "Pine8", "DeciduousR", "Deciduous1", "Deciduous2", "Deciduous3", "Deciduous4", "Deciduous5", "Deciduous6", "Deciduous7", "Deciduous8",  "MixedwoodR", "Mixedwood1", "Mixedwood2", "Mixedwood3", "Mixedwood4", "Mixedwood5", "Mixedwood6", "Mixedwood7", "Mixedwood8", "BlackSpruceR", "BlackSpruce1", "BlackSpruce2", "BlackSpruce3", "BlackSpruce4", "BlackSpruce5", "BlackSpruce6", "BlackSpruce7", "BlackSpruce8", "TreedFen", "TreedSwamp", "ShrubbySwamp", "ShrubbyBog",  "ShrubbyFen", "GraminoidFen", "Marsh", "Shrub", "GrassHerb",  "Crop", "TameP", "RoughP", "Wellsites", "Rural", "Urban", "Industrial", "MineV", "EnSoftLin", "TrSoftLin", "EnSeismic")))

## X.4 Loop through species ----
for(i in 1:nrow(spp)){
  
  ## X.5 Filter data ----
  coef.i <- dplyr::filter(coef.sum, species==spp$species[i])
  
  ## X.6 Plot ----
  plot.i <- ggplot(coef.i) + 
    geom_errorbar(aes(x=var, ymin = mn-1.96*se, ymax = mn+1.96*se), colour="grey30") +
    geom_point(aes(x=var, y=mn),alpha=0.7, size=2, colour="grey30") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          legend.position ="bottom",
          legend.title = element_blank()) +
    xlab("") +
    ylab("Density (birds/ha)") +
    ggtitle(spp$species[i])

  ## X.7 Save ----
  ggsave(file.path(root, "Deviation From Expected", "Figures", "Suitability_species", paste0(spp$species[i], ".jpg")), width = 12, height = 4)
  
  cat(spp$species[i], "  ")
  
}