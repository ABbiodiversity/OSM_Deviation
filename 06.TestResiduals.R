# ---
# title: "06.TestResiduals"
# author: "Elly Knight"
# created: "2025-03-03"
# inputs: model predictions
# outputs: explanation of residuals
# notes:

# ---

# 1. Setup ----

## 1.1 Load packages ----
library(tidyverse) # data manipulation and visualization

## 1.2 Set GD roots----
root <- "G:/Shared drives/ABMI_ECKnight/Projects/OSM/Deviation From Expected"

## 1.3 Load test data----
load(file.path(root, "Data", "Test.Rdata"))

# 2. Wrangle predictions ----

## 2.1 Get list of predictions ----
pred_files <- data.frame(file = list.files(file.path(root, "Results", "Predictions"), pattern="*.csv", recursive=TRUE)) |>
  separate(file, into=c("prediction", "spf", "species", "bootstrap", "filetype")) |>
  cbind(path = list.files(file.path(root, "Results", "Predictions"), pattern="*.csv", recursive = TRUE, full.names = TRUE)) |> 
  dplyr::select(-filetype, -spf, -prediction)

## 2.2 Read them in ----
pred <- map_dfr(read.csv, .x=pred_files$path)

## 2.3 Bind to the covariates ----
pred_covs <- pred |> 
  inner_join(covs_test)

# 3. Data presents ----

## 3.1 Year ----
ggplot(pred_covs) + 
  geom_point(aes(x=year, y=residual)) +
  geom_smooth(aes(x=year, y = residual))

## 3.2 