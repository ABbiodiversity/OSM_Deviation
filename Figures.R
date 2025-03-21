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

# 2. Study area figures ----

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
  
  

