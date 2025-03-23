# ---
# title: "10.Explore"
# author: "Elly Knight"
# created: "2025-03-03"
# inputs: BRTs of model deviation
# outputs: predictor importance, marginal predictions, and interaction strength from models
# notes:

# ---

# 1. Setup ----

## 1.1 Load packages ----
library(tidyverse) # data manipulation and visualization
library(mgcv) # GAMs

## 1.2 Set GD roots----
root <- "G:/Shared drives/ABMI_ECKnight/Projects/OSM/Deviation From Expected"

## 1.3 Make new names for the covs ----
names <- data.frame(var = c("badr_highwells", "badr_linear", "badr_lowwells", "badr_mine", "badr_minebuffer", "badr_roads", "cei", "propallwel","proppipe", "propmine", "proproad", "propseismi", "year"),
                    scale = c(rep("JEM", 6), rep("LU", 6), "time"),
                    name = c("High impact wellsites", "Linear features", "Low impact wellsites", "Mine/plant", "Mine/plant buffer", "Roads",
                             "Cumulative footprint index", "Wellsites",  "Pipelines", "Mine/plant", "Road", "Seismic lines",
                             "Year"))

# 2. Load interpretation output ----

## 2.1 List of files ----
interp <- data.frame(path = list.files(file.path(root, "Results", "Interpretation"), pattern="*.Rdata", recursive=TRUE, full.names = TRUE),
                     file = list.files(file.path(root, "Results", "Interpretation"), pattern="*.Rdata", recursive = TRUE)) |>
  separate(file, into=c("interpretation", "spf", "species", "bootstrap", "filetype")) |>
  dplyr::select(-filetype, -spf, -interpretation)

## 2.2 Set up loop ----
int.list <- list()
pred.list <- list()
var.list <- list()
for(i in 1:nrow(interp)){
  
  ## 2.3 Load file ----
  load(interp$path[i])
  
  ## 2.4 Add to lists ----
  var.list[[i]] <- var.i |> 
    mutate(species = interp$species[i],
           boot = interp$bootstrap[i])
  int.list[[i]] <- int.i |> 
    mutate(species = interp$species[i],
           boot = interp$bootstrap[i])
  pred.list[[i]] <- pred.i  |> 
    mutate(species = interp$species[i],
           boot = interp$bootstrap[i])
  
}

## 2.5 Collapse list ---
var <- do.call(rbind, var.list)
int <- do.call(rbind, int.list)
pred <- do.call(rbind, pred.list)

## 2.6 Save for later ----
write.csv(var, file.path(root, "Results", "BRTVariableImportance.csv"))
write.csv(int, file.path(root, "Results", "BRTInteractions.csv"))
write.csv(pred, file.path(root, "Results", "BRTPredictions.csv"))

# 3. Variable importance ----

## 3.1 Visualize ----
#looking at spread across boots
ggplot(var |> 
         dplyr::filter(species!="CONI")) +
  geom_point(aes(x=var, y=rel.inf)) +
  facet_wrap(~species, scales = "free_y") +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))

## 3.2 Summarize ----
var.sum <- var |> 
  group_by(species, var) |> 
  summarize(inf.mn = mean(rel.inf),
            inf.se = sd(rel.inf)/(sqrt(25))) |> 
  ungroup() |> 
  mutate(inf.lw = inf.mn - inf.se,
         inf.up = inf.mn + inf.se) |> 
  left_join(names)

## 3.3 Make a figure ----
ggplot(var.sum) +
  geom_errorbar(aes(x=name, ymin = inf.lw, ymax = inf.up, group = species), colour="grey30", width = 0.3) +
  geom_point(aes(x = name, y=inf.mn, colour = species)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1)) +
  xlab("") +
  facet_wrap(~scale, scales="free_x")

# 4. Variable interactions ----

## 4.1 Summarize ----

int.sum <- int |> 
  dplyr::select(-var1.index, -var2.index) |> 
  mutate(row = row_number()) |> 
  pivot_longer(var1.names:var2.names, names_to="id", values_to="var") |> 
  arrange(row, desc(var)) |> 
  pivot_wider(names_from=id, values_from=var) |> 
  mutate(vars = paste0(var2.names, " : ", var1.names)) |> 
  dplyr::filter(!is.na(var1.names)) |> 
  group_by(species, vars) |> 
  summarize(int.mn = mean(int.size),
            int.se = sd(int.size)/(sqrt(25))) |> 
  ungroup() |> 
  dplyr::filter(int.mn > 0.01)

#5. Predictions ----

## 5.1 Pick most important variables ----
#let's take the top 4 for now
var.use <- var.sum |> 
  arrange(species, -inf.mn) |> 
  group_by(species) |> 
  mutate(order = row_number()) |>  
  ungroup() |> 
  dplyr::filter(order %in% c(1:4)) 

summary(var.use$inf.mn)

## 5.2 Make the predictions long ----
pred.long <- pred |> 
  pivot_longer(year:badr_roads, names_to="var2", values_to="val") |>  
  dplyr::filter(var==var2)

## 5.2 Join to the bootstrapped values ----
pred.boot <- inner_join(pred.long, var.use |> 
                          dplyr::select(species, var))

## 5.3 Set up a loop for GAMs ----
pred.list <- list()
for(i in 1:nrow(var.use)){
  
  species.i <- var.use$species[i]
  var.i <- var.use$var[i]
  
  ## 5.4 Get the bootstraps ----
  pred.i <- pred.boot |> 
    dplyr::filter(species==species.i, var==var.i)
  
  ## 5.5 GAM it ----
  gam.i <- gam(pred ~ s(val), data=pred.i)
  
  ## 5.6 GAM predictions ----
  pred.list[[i]] <- predict(gam.i, se.fit=TRUE) |> 
    cbind(pred.i) |> 
    mutate(species = species.i,
           var = var.i)
  
}

## 5.7 Collapse output list ----
gampred <- do.call(rbind, pred.list)

##5.8 Make a plot ----

ggplot(gampred) +
  geom_line(aes(x=val, y=fit)) +
  facet_wrap(species ~ var, scales="free")

ggsave(file.path(root, "Figures", "DeviationDataPresent.jpeg"), width = 15, height = 10)

#6. Deviation explained -----

## 6.1 Get list of model performance files ----
perf.files <- data.frame(path = list.files(file.path(root, "Results", "BRTs", "ModelPerformance"), pattern="*.csv", full.names = TRUE))

## 6.2 Read them in ----
perf <- map_dfr(read.csv, .x=perf.files$path)

## 6.3 Calculate deviance explained ----
perf.sum <- perf |> 
  mutate(dev = (deviance.mean - resid)/deviance.mean) |> 
  group_by(species) |> 
  summarize(dev.mn = mean(dev),
            dev.se = sd(dev)/(sqrt(25))) |> 
  ungroup()