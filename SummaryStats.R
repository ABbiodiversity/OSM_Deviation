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

## 4.6 Deviation ----
pred.sum |> 
  dplyr::filter(mn < 0) |> 
  summary()

pred.sum |> 
  mutate(lwr = mn - q.96*se)

# 5. Deviation explained ----

## 5.1 Get list of model performance files ----
perf.files <- data.frame(path = list.files(file.path(root, "Results", "BRTs", "ModelPerformance"), pattern="*.csv", full.names = TRUE))

## 5.2 Read them in ----
perf <- map_dfr(read.csv, .x=perf.files$path)

## 5.3 Calculate deviance explained ----
perf.sum <- perf |> 
  mutate(dev = (deviance.mean - resid)/deviance.mean) |> 
  group_by(species) |> 
  summarize(dev.mn = mean(dev),
            dev.se = sd(dev)/(sqrt(25))) |> 
  ungroup() |> 
  dplyr::filter(species!="CONI")
mean(perf.sum$dev.mn)
sd(perf.sum$dev.mn)

# 6. Landscape scale effects ----

## 6.1 Get the variables importance ----
var <- read.csv(file.path(root, "Results", "BRTVariableImportance.csv"))

## 6.2 Classify vars ----
names <- data.frame(var = c("badr_highwells", "badr_linear", "badr_lowwells", "badr_mine", "badr_minebuffer", "badr_roads", "cei", "propallwel","proppipe", "propmine", "proproad", "propseismi", "year"),
                    scale = c(rep("JEM", 6), rep("LU", 6), "time"),
                    name = c("High impact wellsites", "Linear features", "Low impact wellsites", "Mine/plant", "Mine/plant buffer", "Roads",
                             "Cumulative footprint index", "Wellsites",  "Pipelines", "Mine/plant", "Road", "Seismic lines",
                             "Year"))

## 6.3 Wrangle to % deviance ----
var.perf <- var |> 
  rename(bootstrap = boot) |> 
  left_join(perf |> 
              mutate(dev = (deviance.mean - resid)/deviance.mean) |> 
              dplyr::select(species, bootstrap, dev),
            multiple="all") |> 
  dplyr::select(-X) |> 
  mutate(var.dev = dev*rel.inf/100) |> 
  group_by(species, var) |> 
  summarize(dev.mn = mean(var.dev),
            dev.se = sd(var.dev)/5) |> 
  ungroup() |> 
  left_join(names)

## 6.4 Just landscape scale ----
var.lu <- var.perf |> 
  dplyr::filter(scale=="LU")

summary(var.lu)

## 6.5 Let's try looking at one ----
pred <- read.csv(file.path(root, "Results", "Predictions.csv"))
load(file.path(root, "Data", "Test.Rdata"))
covs_use <- covs_badr |> 
  rename(badr_highwells = `High Activity Insitu Well Pads`,
         badr_roads = Roads,
         badr_mine = `Plant/Mine`,
         badr_linear = `Dense Linear Features`,
         badr_lowwells = `Low Activity Well Pads`,
         badr_minebuffer = `Plant/Mine Buffer`,
         badr_reference = `Low Disturbance/Reference`) |> 
  dplyr::select(surveyid, year, cei, propmine, proproad, propseismi, propallwel, proppipe, badr_highwells, badr_roads, badr_mine, badr_linear, badr_lowwells, badr_minebuffer)

pred.eg <- pred |> 
  dplyr::filter(species=="OVEN") |> 
  left_join(covs_use) |> 
  mutate(cei_c = case_when(cei < 0.4 ~ "low",
                           cei > 0.8 ~ "high"),
         badr_linear_c = case_when(badr_linear < 0.1 ~ "low",
                                   badr_linear < 0.8 ~ "high"),
         badr_lowwells_c = case_when(badr_lowwells < 0.1 ~ "low",
                                     badr_lowwells > 0.9 ~ "high"),
         propmine_c = case_when(propmine > 0.02 ~ "high",
                                propmine < 0.02 ~ "low"))

ggplot(pred.eg) +
  geom_point(aes(x=propmine, y=residual)) +
  geom_smooth(aes(x=propmine, y=residual), method="lm")

ggplot(pred.eg) +
  geom_boxplot(aes(x=badr_lowwells_c, y=residual))

t.test(residual ~ badr_linear_c, data=pred.eg)

hist(pred.eg$residual)
