# ---
# title: "09. Compare predictions between test and forecast"
# author: "Elly Knight"
# created: "2025-03-23"
# inputs: "output from `08.MakePredictions.R`
# outputs: "paired t-test results"
# notes:

# ---

# 1. Setup ----

## 1.1 Load packages ----
library(tidyverse) # data manipulation and visualization

## 1.2 Set GD roots----
root <- "G:/Shared drives/ABMI_ECKnight/Projects/OSM/Deviation From Expected"

## 1.3 Scientific notation ----
options(scipen=999999)

## 1.4 Load data----
load(file.path(root, "Data", "Test.Rdata"))

# 2. Wrangle ----

## 2.1 Get list of completed predictions ----
done <- data.frame(file = list.files(file.path(root, "Results", "Predictions"), pattern="*.csv", recursive=TRUE),
                   path = list.files(file.path(root, "Results", "Predictions"), pattern="*.csv", recursive=TRUE, full.names = TRUE)) |>
  separate(file, into=c("prediction", "spf", "species", "bootstrap", "filetype")) |>
  dplyr::select(-filetype, -spf, -prediction)

## 2.2 Read in predictions ----
pred_raw <- map_dfr(read.csv, .x=done$path)

## 2.3 Identify test vs forecast ----
pred <- pred_raw |> 
  left_join(covs_test |> 
              dplyr::select(surveyid, year)) |> 
  mutate(dataset = ifelse(year < 2020, "test", "forecast"))

## 2.4 Save for later ----
write.csv(pred, file.path(root, "Results", "Predictions.csv"), row.names = FALSE)

# 3. Compare between test and forecast -----

## 3.1 Summarize to percent diff per bootstrap ----
pred.sum <- pred |> 
  group_by(dataset, species, boot) |> 
  summarize(diff_mn = mean(residual),
            pred_mn = mean(prediction)) |> 
  ungroup() |> 
  mutate(perc_mn = diff_mn/pred_mn*100)

## 3.2 Visualize ----
ggplot(pred.sum) +
  geom_hline(aes(yintercept=0), linetype="dashed") +
  geom_violin(aes(x=species, y=perc_mn, colour=dataset))

## 3.3 Paired T-Test for difference ----
spp <- unique(pred.sum$species)

out <- data.frame()
for(i in 1:length(spp)){
  
  pred.i <- dplyr::filter(pred.sum, species==spp[i]) |> 
    dplyr::select(-pred_mn) |> 
    pivot_wider(names_from=dataset, values_from=c(perc_mn, diff_mn))
  
  t_perc <- t.test(pred.i$perc_mn_forecast, pred.i$perc_mn_test, paired=TRUE)
  t_diff <- t.test(pred.i$diff_mn_forecast, pred.i$diff_mn_test, paired=TRUE)
  
  out <- data.frame(t = c(t_perc$statistic, t_diff$statistic),
                      p = c(t_perc$p.value, t_diff$p.value),
                      diff = c(t_perc$estimate, t_diff$estimate),
                      low = c(t_perc$conf.int[1], t_diff$conf.int[1]),
                      high = c(t_perc$conf.int[2], t_diff$conf.int[2]),
                      measure = c("Percent", "Density"),
                      species = rep(spp[i], 2)) |> 
    rbind(out)
  
}

## 3.4 Save results ----
write.csv(out, file.path(root, "Results", "PredictionComparison.csv"), row.names = FALSE)