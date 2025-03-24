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

## 1.2 Set GD roots----
root <- "G:/Shared drives/ABMI_ECKnight/Projects/OSM/Deviation From Expected"

## 1.3 Scientific notation ----
options(scipen=999999)

# 2. Data summary ----

## 2.1 Get the data -----
load(file.path(root, "Data", "Train.Rdata"))
load(file.path(root, "Data", "Test.Rdata"))

## 2.2 Number of surveys ----
covs_train |> 
  dplyr::filter(use=="train") |> 
  nrow()

nrow(covs_test)

## 2.3 Number of unique locations ----
covs_train |> 
  dplyr::filter(use=="train") |> 
  dplyr::select(Easting, Northing) |> 
  unique() |> 
  nrow()

covs_test |> 
  dplyr::select(Easting, Northing) |> 
  unique() |> 
  nrow()

## 2.4 Number of detections ----
summary(bird_test)

#Per species
covs_train |> 
  dplyr::filter(use=="train") |> 
  dplyr::select(surveyid) |> 
  left_join(bird_train) |> 
  dplyr::select(all_of(colnames(bird_test))) |> 
  pivot_longer(-surveyid, names_to="species", values_to="count") |> 
  dplyr::filter(count > 0) |> 
  group_by(species) |> 
  summarize(n=n())

bird_test |> 
  pivot_longer(-surveyid, names_to="species", values_to="count") |> 
  dplyr::filter(count > 0) |> 
  group_by(species) |> 
  summarize(n=n())

#Across species
covs_train |> 
  dplyr::filter(use=="train") |> 
  dplyr::select(surveyid) |> 
  left_join(bird_train) |> 
  dplyr::select(all_of(colnames(bird_test))) |> 
  pivot_longer(-surveyid, names_to="species", values_to="count") |> 
  dplyr::filter(count > 0) |> 
  group_by(species) |> 
  summarize(n=n()) |> 
  ungroup() |> 
  summarize(mn = mean(n),
            sd = sd(n))

bird_test |> 
  pivot_longer(-surveyid, names_to="species", values_to="count") |> 
  dplyr::filter(count > 0) |> 
  group_by(species) |> 
  summarize(n=n()) |> 
  ungroup() |> 
  summarize(mn = mean(n),
            sd = sd(n))

## 2.5 Counts per detection ----
covs_train |> 
  dplyr::filter(use=="train") |> 
  dplyr::select(surveyid) |> 
  left_join(bird_train) |> 
  dplyr::select(all_of(colnames(bird_test))) |> 
  pivot_longer(-surveyid, names_to="species", values_to="count") |> 
  dplyr::filter(count > 0) |> 
  summarize(min = min(count),
            max = max(count),
            mn = mean(count),
            sd = sd(count))

bird_test |> 
  pivot_longer(-surveyid, names_to="species", values_to="count") |> 
  dplyr::filter(count > 0) |> 
  summarize(min = min(count),
            max = max(count),
            mn = mean(count),
            sd = sd(count))

# 3. Habitat models -----

## 3.1 Get coefs & predictions ----
coef <- read.csv(file.path(root, "Results", "Coefficients.csv"))

# 4. Deviation -----

## 4.1 Read in predictions ----
pred <- read.csv(file.path(root, "Results", "Predictions.csv"))

## 4.2 Summarize ----
pred.sum <- pred |> 
  dplyr::filter(species!="CONI") |> 
  group_by(dataset, species, boot) |> 
  summarize(diff_mn = mean(residual),
            pred_mn = mean(prediction)) |> 
  ungroup() |> 
  mutate(perc_mn = diff_mn/pred_mn*100) |> 
  group_by(species, dataset) |> 
  summarize(mn = mean(perc_mn),
            se = sd(perc_mn)/5) |> 
  ungroup() |> 
  mutate(lwr = mn - 1.96*se,
         upr = mn + 1.96*se)

## 4.3 Deviation across species by dataset ----
pred.sum |> 
  group_by(dataset) |> 
  summarize(mean = mean(mn),
            sd = sd(mn))

## 4.4 Read in t-tests ----
ts <- read.csv(file.path(root, "Deviation From Expected", "Results", "PredictionComparison.csv")) |> 
  dplyr::filter(measure=="Percent") 

## 4.5 Summarize ----
ts |> 
  mutate(sig = ifelse(p < 0.05, 1, 0)) |> 
  summarize(sigs = sum(sig))

ts |> 
  mutate(dir = ifelse(diff < 0, "neg", "pos")) |> 
  group_by(dir) |> 
  summarize(count = n())

## 4.6 Tidy for appendix 2
app2 <- ts |> 
  mutate(t = round(t, 2),
         p = round(p, 3),
         diff = round(diff, 2),
         low = round(low, 2),
         high = round(high, 2)) |> 
  mutate(difference = paste0(diff, " (", low, ", ", high, " 95% CI)")) |> 
  dplyr::select(species, t, p, difference) 

write.csv(app2, file.path(root, "Deviation From Expected", "Results", "Appendix2.csv"), row.names = FALSE)

# 5. Correlates of deviation from expected ----

## 5.1 Get the summary ----
load(file.path(root, "Results", "ModelsOfDeviation.Rdata"))

## 5.2 Summarize across species & vars ----
mean(out$estimate)
sd(out$estimate)

## 5.3 R2 ----
out |> 
  dplyr::select(species, r2) |> 
  unique()
