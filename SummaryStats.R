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

## 3.1 Get list of landcover models----
todo <- data.frame(file = list.files(file.path(root, "Results", "LandCoverModels", "Models"), pattern="*.Rdata", recursive = TRUE)) |>
  separate(file, into=c("f1", "f2", "species", "bootstrap", "f3")) |> 
  dplyr::select(species, bootstrap) |>
  inner_join(data.frame(file = list.files(file.path(root, "Results", "LandCoverModels", "Coefficients"), pattern="*.csv", recursive = TRUE)) |>
               separate(file, into=c("f4", "f5", "species", "bootstrap", "f6")) |>
               dplyr::select(species, bootstrap))

## 3.2 Set up loop ----
terms.list <- list()
coefs.list <- list()
for(i in 1:nrow(todo)){
  
  boot.i <- todo$bootstrap[i]
  species.i <- as.character(todo$species[i])
  
  ## 3.3 Load model ----
  load(file.path(root, "Results", "LandcoverModels", "Models", species.i, paste0("NorthModel_", species.i, "_", boot.i, ".Rdata")))
  
  ## 3.4 Get terms ----
  terms.list[[i]] <- data.frame(terms = as.character(attr(bestmodel[["terms"]], "variables")),
                        bootstrap = boot.i,
                        species = species.i) |> 
    dplyr::filter(!terms %in% c("list", "count", "offset(offset)"))
  
  ## 3.5 Get coeffs ----
  coefs.list[[i]] <- data.frame(bestmodel$coefficients) |> 
    mutate(bootstrap = boot.i,
           species = species.i) |> 
    rownames_to_column() |> 
    rename(var = rowname, coef = bestmodel.coefficients) |> 
    dplyr::filter(var!="(Intercept)")
  
  cat(i, "  ")
    
}

## 3.6 Tidy results ----
terms <- do.call(rbind, terms.list)
coefs <- do.call(rbind, coefs.list)

## 3.7 Summarize -----
coefs.sum <- coefs |> 
  group_by(species, var) |> 
  summarize(mn = mean(coef),
            se = sd(coef)/5) |> 
  ungroup()

## 3.8 Plot it -----
ggplot(coefs.sum |> dplyr::filter(var!="climate", species!="CONI")) + 
  geom_errorbar(aes(x=var, ymin = mn-1.96*se, ymax = mn+1.96*se, group=species)) +
  geom_point(aes(x=var, y=mn, colour=species)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

#TO DO: DECIDE WHETHER TO PACKAGE THESE... PROBABLY SHOULD


# 4. Deviation -----

## 4.1 Get list of completed predictions ----
done <- data.frame(file = list.files(file.path(root, "Results", "Predictions"), pattern="*.csv", recursive=TRUE),
                   path = list.files(file.path(root, "Results", "Predictions"), pattern="*.csv", recursive=TRUE, full.names = TRUE)) |>
  separate(file, into=c("prediction", "spf", "species", "bootstrap", "filetype")) |>
  dplyr::select(-filetype, -spf, -prediction)

## 4.2 Read in predictions ----
pred <- map_dfr(read.csv, .x=done$path)

## 4.3 Save this for figures ----
write.csv(pred, file.path(root, "Results", "Predictions.csv"), row.names = FALSE)

## 4.4 Summarize ----
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
  mutate(lwr = mn - 1.96*se,
         upr = mn + 1.96*se)

pred.sum |> 
  dplyr::filter(mn < 0) |> 
  summary()

pred.sum |> 
  mutate(lwr = mn - q.96*se)