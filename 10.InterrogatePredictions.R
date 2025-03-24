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

# 2. Binarize BADR & LU covs ----

## 2.1 Tidy names etc ----
covs_use <- covs_badr |> 
  rename(badr_highwells = `High Activity Insitu Well Pads`,
         badr_roads = Roads,
         badr_mine = `Plant/Mine`,
         badr_linear = `Dense Linear Features`,
         badr_lowwells = `Low Activity Well Pads`,
         badr_minebuffer = `Plant/Mine Buffer`,
         badr_reference = `Low Disturbance/Reference`) |> 
  dplyr::select(surveyid, cei, propmine, proproad, propseismi, propallwel, proppipe, badr_highwells, badr_roads, badr_mine, badr_linear, badr_lowwells, badr_minebuffer) |> 
  pivot_longer(-c(surveyid), names_to="var", values_to="val")

## 2.2 Use quantiles for landscape variables ----
covs_lu <- covs_use |> 
  dplyr::filter(var %in% c("cei", "propmine", "proproad", "propseismi", "propallwel", "proppipe")) |> 
  group_by(var) |> 
  mutate(class = case_when(val < quantile(val, 0.2) ~ "low",
                           val >= quantile(val, 0.2) & val <= quantile(val, 0.8) ~ "medium",
                           val > quantile(val, 0.8) ~ "high")) |> 
  ungroup()

table(covs_lu$var, covs_lu$class)

## 2.3 Use values for local variables ----
covs_jem <- covs_use |> 
  dplyr::filter(var %in% c("badr_highwells", "badr_roads", "badr_linear", "badr_lowwells", "badr_minebuffer")) |> 
  mutate(class = case_when(val==0 ~ "low",
                           val > 0 & val <= 0.8 ~ "medium",
                            val > 0.8 ~ "high"))

table(covs_jem$var, covs_jem$class)

## 2.4 Put together ----
covs_class <- rbind(covs_lu, covs_jem) |> 
  dplyr::select(-val) |> 
  mutate(class = factor(class, levels = c("low", "medium", "high"))) |> 
  pivot_wider(names_from=var, values_from=class)

# 3. Model deviation by oil and gas features ----

## 3.2 Wrangle predictions ----
pred_use <- pred |> 
  dplyr::filter(dataset=="forecast")

## 3.1 Set up species loop ----
spp <- unique(pred$species)

lm.list <- list()
dredge.list <- list()
out.list <- list()
dat.list <- list()
for(i in 1:length(spp)){
  
  ## 3.2 Filter data
  pred.i <- dplyr::filter(pred_use, species==spp[i]) |> 
    group_by(surveyid) |> 
    summarize(residual_mn = mean(residual)) |> 
    ungroup() |> 
    left_join(covs_class)
  
  dat.list[[i]] <- pred.i
  
  ## 3.3 Try a model ----
  lm.i <- lm(residual_mn ~ cei + propmine + proproad + propseismi + propallwel + proppipe + badr_highwells + badr_roads + badr_linear + badr_lowwells + badr_minebuffer, data=pred.i, na.action="na.fail")
  
  ## 3.4 Dredge it ----
  dredge.list[[i]] <- dredge(lm.i)
  
  ## 3.5 Most parsimonious within delta 2 ----
  dredge.i <- data.frame(dredge.list[[i]]) |> 
    mutate(mod = row_number()) |> 
    dplyr::filter(delta < 2) |>
    dplyr::filter(df==min(df)) |> 
    dplyr::filter(delta==min(delta))
  
  ## 3.6 Get the model ----
  lm.list[[i]] <- get.models(dredge.list[[i]], subset=dredge.i$mod)[[1]]
  
  ## 3.7 Summarize ----
  sum.i <- summary(lm.list[[i]])
  out.list[[i]] <- data.frame(sum.i$coefficients) |> 
    rownames_to_column() |> 
    mutate(weight = dredge.list[[i]]$weight[1],
           df = dredge.list[[i]]$df[1],
           R2 = sum.i$adj.r.squared,
           species = spp[i])
  
  cat(i , "  ")
  
}

## 3.8 Name the lists & collapse ----
names(lm.list) <- spp
names(dredge.list) <- spp
dat <- do.call(rbind, dat.list)
out_raw <- do.call(rbind, out.list)
colnames(out_raw) <- c("var", "estimate", "se", "t", "p", "weight", "df", "r2", "species")

## 3.9 Tidy the summary ----
out <- out_raw |> 
  mutate(class = str_sub(var, -4, -1),
         class = ifelse(class=="high", "high", "medium")) |> 
  dplyr::filter(class=="high",
                p < 0.05) |> 
  mutate(var = str_sub(var, -100, -5))

## 3.10 Join back to the data ----
dat.out <- dat |> 
  pivot_longer(-c(surveyid, residual_mn), names_to="var", values_to="val") |> 
  inner_join(out, relationship="many-to-many") |> 
  dplyr::filter(val!="medium")

## 3.11 Plot ----
ggplot(dat.out |> dplyr::filter(species=="OVEN")) +
  geom_boxplot(aes(x=val, y=log(residual_mn), fill=estimate))+
  facet_wrap(~var) +
  scale_fill_gradient2()


## 3.9 Save ----
save(lm.list, dredge.list, out, file.path(root, "Results", "ModelsOfDeviation.Rdata"))

# 4. Model deviation by year -----

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