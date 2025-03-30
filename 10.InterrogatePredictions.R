# ---
# title: "08. Interrogate forecast predictions"
# author: "Elly Knight"
# created: "2025-03-23"
# inputs: "output from `08.MakePredictions.R`
# outputs: none
# notes:

# ---

# 1. Setup ----

## 1.1 Load packages ----
library(tidyverse) # data manipulation and visualization
library(lme4) # mixed models
library(MuMIn) # dredging

## 1.2 Set GD roots----
root <- "G:/Shared drives/ABMI_ECKnight/Projects/OSM/Deviation From Expected"

## 1.3 Scientific notation ----
options(scipen=999999)

## 1.4 Load test data----
load(file.path(root, "Data", "Test.Rdata"))

## 1.5 Load predictions ----
pred <- read.csv(file.path(root, "Results", "Predictions.csv")) 

# 2. Wrangling ----

## 2.1 Tidy names etc ----
covs_use <- covs_badr |> 
  rename(badr_highwells = `High Activity Insitu Well Pads`,
         badr_roads = Roads,
         badr_mine = `Plant/Mine`,
         badr_linear = `Dense Linear Features`,
         badr_lowwells = `Low Activity Well Pads`,
         badr_minebuffer = `Plant/Mine Buffer`,
         badr_reference = `Low Disturbance/Reference`) |> 
  dplyr::select(surveyid, cei, proproad, propseismi, propallwel, proppipe, badr_highwells, badr_roads, badr_linear, badr_lowwells, badr_minebuffer)

## 2.2 Wrangle predictions ----
pred_use <- pred |> 
  dplyr::filter(dataset=="forecast") |> 
  group_by(species, surveyid) |> 
  summarize(residual_mn = mean(residual)) |> 
  ungroup()

# 3. Model deviation by oil and gas features ----

## 3.1 Set up species loop ----
spp <- unique(pred$species)

lm.list <- list()
out.list <- list()
dat.list <- list()
for(i in 1:length(spp)){
  
  ## 3.2 Filter data
  pred.i <- dplyr::filter(pred_use, species==spp[i]) |> 
    left_join(covs_use)
  
  dat.list[[i]] <- pred.i
  
  ## 3.3 Try a model ----
  lm.list[[i]] <- lm(residual_mn ~ cei + proproad + propseismi + propallwel + proppipe + badr_highwells + badr_roads + badr_linear + badr_lowwells + badr_minebuffer, data=pred.i, na.action="na.fail")
  
  ## 3.7 Summarize ----
  sum.i <- summary(lm.list[[i]])
  out.list[[i]] <- data.frame(sum.i$coefficients) |> 
    rownames_to_column() |> 
    mutate(R2 = sum.i$adj.r.squared,
           species = spp[i])
  
  cat(i , "  ")
  
}

## 3.8 Name the lists & collapse ----
names(lm.list) <- spp
out_raw <- do.call(rbind, out.list)
colnames(out_raw) <- c("var", "estimate", "se", "t", "p", "r2", "species")

## 3.9 Tidy the summary ----
out <- out_raw |> 
  dplyr::filter(var!="(Intercept)") |> 
  mutate(sig = ifelse(p < 0.01, "significant", "nonsignificant"))

## 3.10 Plot ----
#ok let's see what we get! The data present of the whole project
ggplot(out |> 
         dplyr::filter(sig=="significant")) + 
  geom_errorbar(aes(x=species, ymin = estimate-1.96*se, ymax = estimate+1.96*se, colour=var)) +
  geom_point(aes(x=species, y=estimate, colour=var)) +
  geom_hline(aes(yintercept=0), linetype="dashed") +
  facet_wrap(~var, scales="free")

## 3.11 Save ----
save(lm.list, dredge.list, out_raw, file=file.path(root, "Results", "ModelsOfDeviation.Rdata"))

# 4. Model deviation by year -----

#seems cute, might delete later

## 4.1 Put together ----
pred_year <- pred |> 
  left_join(covs_test |> 
              dplyr::select(surveyid, year)) |> 
  group_by(species, surveyid, year) |> 
  summarize(residual_mn = mean(residual)) |> 
  ungroup()

## 4.2 Visualize ----
ggplot(pred_year) + 
#  geom_point(aes(x=year, y=residual_mn)) +
  geom_smooth(aes(x=year, y=residual_mn), method="gam") +
  geom_hline(aes(yintercept=0)) +
  geom_vline(aes(xintercept=2020)) +
  facet_wrap(~species, scales="free")