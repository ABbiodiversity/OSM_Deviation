# ---
# title: "01.NewData"
# author: "Elly Knight"
# created: "2025-03-03"
# inputs: project_ids of interest to add to dataset
# outputs: main report for project_ids
# notes: this script is based on the `01.GetWildTraxData.R` and `03.HarmonizaData.R` scripts in the BirdModels repo.

# ---

# 1. Setup ----

## 1.1 Load packages ----
library(tidyverse) # data manipulation and visualization
library(wildrtrax) # data download

## 1.2 Set GD roots----
root <- "G:/Shared drives/ABMI_ECKnight/Projects/OSM/Deviation From Expected"

## 1.3 Login to WildTrax----
config <- "00.WTlogin.R"
source(config)

## 1.4 Authenticate----
wt_auth()

# 2. Download ----

## 2.1 List of projects ----
projects <- c(2088, 1174, 1281)

## 2.2 Download them -----
dat.list <- list()
for(i in 1:length(projects)){
  
  dat.list[[i]] <- wt_download_report(projects[i], sensor="ARU", weather_cols = FALSE, report = "main")
  
}

## 2.3 Collapse list ----

dat <- do.call(rbind, dat.list)

# 3. Tidy ----

# 3.1 Replace tmtt, make wide ----
dat_wide <- dat |> 
  wt_tidy_species(remove=c("mammal", "amphibian", "abiotic", "insect", "human", "unknown")) |> 
  wt_replace_tmtt() |> 
  wt_make_wide()

# 4. QPAD ----
  
# 4.1 Get some offsets ----
dat_qpad <- dat_wide |> 
  rename(GRAJ = CAJA) |> 
  wt_qpad_offsets() |> 
  rename(CAJA = GRAJ)

# 4.2 Get remaining species ----
spp_mean <- colnames(dat_wide |> 
           dplyr::select(ALFL:YRWA)) |> 
  setdiff(colnames(dat_qpad))

# 4.3 Calculate mean offset for them ----
offset_mean <- rowMeans(dat_qpad)
offset_df <- data.frame(replicate(offset_mean, n=length(spp_mean)))
colnames(offset_df) <- spp_mean

# 4.4 Put togther ----
dat_off <- cbind(dat_qpad, offset_df)
dat_off <- dat_off[, order(colnames(dat_off))]

# 5. Save ----
save(dat_wide, dat_off, file = file.path(root, "Data", "NewWTData.Rdata"))
