# ---
# title: "02.GetAdditionalTestData"
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
projects <- c(2088, 1174, 1281, 1240, 2132)

## 2.2 Download them -----
dat.list <- list()
for(i in 1:length(projects)){
  
  dat.list[[i]] <- wt_download_report(projects[i], sensor="ARU", weather_cols = FALSE, report = "main")
  
}

## 2.3 Collapse list ----

dat <- do.call(rbind, dat.list)

# 3. Tidy ----

# 3.1 Replace tmtt, make wide ----
# quick and dirty filter of lat lon to get to the AOI
# filter to 2020 and on
dat_wide <- dat |> 
  wt_tidy_species(remove=c("mammal", "amphibian", "abiotic", "insect", "human", "unknown")) |> 
  wt_replace_tmtt() |> 
  mutate(species_code = ifelse(species_code=="GRAJ", "CAJA", species_code)) |>
  wt_make_wide() |>
  mutate(recording_date_time = ymd_hms(recording_date_time),
         year = year(recording_date_time)) |> 
  dplyr::filter(!is.na(latitude), !is.na(year),
                latitude > 52.9,
                longitude > -119.4,
                year >= 2020)

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

# 5. Save locations for gis ----

## 5.1 Get the locations ----
dat_locs <- dat_wide |> 
  dplyr::select(location_id, latitude, longitude, year) |> 
  unique() |> 
  mutate(gisid = paste0(location_id, "_", year),
         buffer = 0,
         topsecret = 0)

write.csv(dat_locs, file.path(root, "Data", "NewWTLocationsForGIS.csv"), row.names = FALSE)

# 6. Save ----
save(dat_wide, dat_off, file = file.path(root, "Data", "NewWTData.Rdata"))
