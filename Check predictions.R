# ---
# title: "Summary Stats"
# author: "Elly Knight"
# created: "2025-03-21"
# inputs: various
# outputs: none
# notes: for calculation of summary stats to go in report

# ---

# 1. Setup ----

## 1.1 Load packages ----
library(tidyverse) # data manipulation and visualization
library(sf)
#library(wildrtrax) # qpad corrections
library(terra) # raster handling for qpad corrections
library(QPAD)

## 1.2 Set GD roots----
root <- "G:/Shared drives/ABMI_ECKnight/Projects/OSM/Deviation From Expected"

## 1.3 Scientific notation ----
options(scipen=999999)

## 1.4 Load data----
load(file.path(root, "Data", "Train.Rdata"))
load(file.path("G:/Shared drives/ABMI_ECKnight/Projects/BirdModels", "Data", "Harmonized.Rdata"))

## 1.5 Load QPAD functions ----
source("00.QPADCorrections.R")

# 2. Wrangle ----

## 2.1 Get the withheld test records from the training dataset----
covs_test <- covs_train |> 
  dplyr::filter(use=="test")

## 2.2 Get duration, fix ebird, etc -----
covs_latlon <- covs_test |> 
  st_as_sf(coords = c("Easting", "Northing"), remove=FALSE, crs=3400) |> 
  st_transform(crs=4326) |> 
  st_coordinates() |> 
  data.frame() |> 
  cbind(covs_test) |> 
  rename(longitude = X, latitude = Y) |> 
  inner_join(use |> 
               dplyr::filter(distance > 150) |> 
              dplyr::select(project_id, gisid, date_time, duration),
            multiple="all") |> 
  rename(task_duration = duration) |> 
  mutate(date_time = case_when(organization=="ebird" ~ date_time + hours(6),
                               organization!="ebird" ~ date_time))

## 2.3 Get the other objects ----
bird_test <- dplyr::filter(bird_train, surveyid %in% covs_test$surveyid)
off_test <- dplyr::filter(off_train, surveyid %in% covs_test$surveyid)

# 3. Get QPAD corrections ----
corr_test <- qpad_correction(covs_latlon,
                             species = c("LEFL", "CMWA", "HETH", "OVEN", "PIWO", "BTNW", "NOWA", "GRAJ", "RUGR", "YBSA", "DEJU", "BHCO", "CAWA", "OSFL")) |> 
  rename(CAJA = GRAJ) |> 
  rowwise() |> 
  mutate(CONI = mean(c_across(everything()))) |> 
  ungroup()

# 4. Make predictions -----

## 4.1 Get list of landcover models----
todo <- data.frame(file = list.files(file.path(root, "Results", "LandCoverModels", "Models"), pattern="*.Rdata", recursive = TRUE)) |>
  separate(file, into=c("f1", "f2", "species", "bootstrap", "f3")) |> 
  dplyr::select(species, bootstrap) |>
  inner_join(data.frame(file = list.files(file.path(root, "Results", "LandCoverModels", "Coefficients"), pattern="*.csv", recursive = TRUE)) |>
               separate(file, into=c("f4", "f5", "species", "bootstrap", "f6")) |>
               dplyr::select(species, bootstrap))

## 4.2 Link functions----
inv.link  <- function (eta) {pmin(pmax(exp(eta), .Machine$double.eps), .Machine$double.xmax)}
link <- poisson()$linkfun

set.seed(1234)
loop <- todo |> 
  group_by(species) |> 
  sample_n(3) |> 
  ungroup()

## 4.3 ----
out.list <- list()
for(i in 1:nrow(loop)){
  
  #1. Loop settings----
  boot.i <- loop$bootstrap[i]
  species.i <- as.character(loop$species[i])
  
  #2. Load the models----
  load(file.path(root, "Results", "ClimateModels", "Models", species.i, paste0("ClimateModel_", species.i, "_", boot.i, ".Rdata")))
  load(file.path(root, "Results", "LandcoverModels", "Models", species.i, paste0("NorthModel_", species.i, "_", boot.i, ".Rdata")))
  
  #3. Get the data----
  bird.i <- bird_test[, species.i]
  off.i <- off_test[, species.i]
  corr.i <- corr_test[, species.i]
  dat.i <- cbind(covs_test, off.i, corr.i)
  colnames(dat.i) <- c(colnames(covs_test), "offset", "correction")
  
  #4. Make the climate predictions----
  dat.i$climate <- inv.link(predict(averagemodel, type="link", full=TRUE, newdata = dat.i))
  
  #5. Make the landcover predictions----
  dat.i$landcover <- predict(bestmodel, type="response", newdata = dat.i)
  
  #6. Truncate & package----
  q99 <- quantile(dat.i$landcover, 0.99)
  
  out.i <- dat.i |> 
    mutate(prediction = ifelse(landcover > q99, q99, landcover)) |> 
    dplyr::select(surveyid, climate, landcover, prediction, correction) |> 
    mutate(count = bird.i,
           pred_count = prediction*correction,
           density = bird.i+0.0000001/correction,
           residual = density - prediction,
           species = species.i,
           boot = boot.i)
  
  out.list[[i]] <- out.i
  
  cat(i, "   ")
  
}

out <- do.call(rbind, out.list)

out.sum <- out |> 
  dplyr::filter(species!="CONI") |> 
  group_by(species, boot) |> 
  summarize(diff_mn = mean(residual),
            pred_mn = mean(prediction)) |> 
  ungroup() |> 
  mutate(perc_mn = diff_mn/pred_mn*100) |> 
  group_by(species) |> 
  summarize(mn = mean(perc_mn),
            se = sd(perc_mn)/(sqrt(3))) |> 
  ungroup() |> 
  mutate(species = factor(species, levels=c("LEFL", "BTNW", "CMWA", "HETH", "OVEN", "PIWO", "NOWA", "CAJA", "DEJU", "RUGR", "YBSA", "BHCO", "CAWA", "OSFL")))

ggplot(out) + 
  geom_hex(aes(x=species, y=residual)) +
  geom_hline(aes(yintercept=0), linetype="dashed")
  
ggplot(out.sum) +
  geom_errorbar(aes(x=species, ymin = mn-1.96*se, ymax = mn+1.96*se), colour="grey30") +
  geom_point(aes(x=species, y=mn), pch=21, alpha=0.7, colour="grey30", size=2) +
  geom_hline(aes(yintercept=0), linetype="dashed") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust=1, vjust=1),
        axis.title.x = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank()) +
  ylab("Percent difference in density between predicted and observed")
