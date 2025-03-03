# ---
# title: "02.WrangleData"
# author: "Elly Knight"
# created: "2025-03-03"
# inputs: stratified data object from provincial model dataset & new downloaded data, some GIS objects
# outputs: filtered test & train data object for OSM modelling and exploring deviation
# notes: this script uses output from the `05.StratifyData.R` script in the BirdModels repo as well as previous scripts in this repo.

# ---

# 1. Setup ----

## 1.1 Load packages ----
library(tidyverse) # data manipulation and visualization
library(sf) # gis

## 1.2 Set GD roots----
rootin <- "G:/Shared drives/ABMI_ECKnight/Projects/BirdModels"
rootout <- "G:/Shared drives/ABMI_ECKnight/Projects/OSM"

## 1.3 Import bird model data ----
load(file.path(rootin,"Data", "Stratified.Rdata"))

## 1.4 Import LU shapefile ----
lu <- read_sf(file.path(rootout, "GIS", "OSR_LU_fxd", "OSR_LUs_fxd.shp"))

## 1.5 Import BADR treatment ----
# badr <- read_sf(file.path(rootout, "GIS", "year_2021_treat.shp")) |> 
#   st_make_valid()

## 1.6 Import the newly downloaded data ----
load(file.path(rootout, "Deviation From Expected", "Data", "NewWTData.Rdata"))

#2. Bird model data preparation ----

## 2.1 Merge LUs to single AOI ----

aoi <- st_union(lu)

## 2.2 Get unique locations * year ----
loc <- covs |> 
  dplyr::select(locationid, Easting, Northing) |> 
  unique()

## 2.3 Make it an sf ----
loc_sf <- loc |> 
  st_as_sf(coords = c("Easting", "Northing"), crs=3400) |> 
  st_transform(raster::crs(aoi))

#3. Bird model data filtering ----

## 3.1 Get the locations within the aoi ----
loc_aoi <- loc_sf |> 
  st_intersection(aoi)

## 3.2 Filter the covs object to aoi ----
covs_aoi <- covs |> 
  dplyr::filter(locationid %in% loc_aoi$locationid)

## 3.3 Split into train and test ----
covs_train <- covs_aoi |> 
  mutate(year = year + 1992) |> 
  dplyr::filter(year < 2020, year >= 2010)

covs_test <- covs_aoi |> 
  mutate(year = year + 1992) |> 
  dplyr::filter(year >= 2020)

## 3.4 Filter the other objects -----

bird_train <- bird |> 
  dplyr::filter(surveyid %in% covs_train$surveyid)

off_train <- off |> 
  dplyr::filter(surveyid %in% covs_train$surveyid)

bird_test <- bird |> 
  dplyr::filter(surveyid %in% covs_test$surveyid)

off_test <- bird |> 
  dplyr::filter(surveyid %in% covs_test$surveyid)
# 
# #4. Add new data to test dataset----
# 
# ## 4.1 Add survey ids ----
# new_wide <- dat_wide |> 
#   mutate(surveyid = row_number() + max(covs$surveyid))
# 
# new_off <- dat_off  |> 
#   mutate(surveyid = row_number() + max(covs$surveyid))
#   
# ## 4.1 Filter to years of interest and AOI ----
# new_aoi <- new_wide |> 
#   dplyr::filter(!is.na(longitude), !is.na(latitude),
#                 aru_task_status=="Transcribed",
#                 task_method=="1SPT") |> 
#   st_as_sf(coords = c("longitude", "latitude"), crs=4326) |> 
#   st_transform(crs=3400) |> 
#   st_coordinates() |> 
#   data.frame() |> 
#   rename(Easting = X, Northing = Y) |> 
#   cbind(new_wide  |> 
#           dplyr::filter(!is.na(longitude), !is.na(latitude),
#                         aru_task_status=="Transcribed",
#                         task_method=="1SPT")) |> 
#   mutate(recording_date_time = ymd_hms(recording_date_time),
#          year = year(recording_date_time)) |> 
#   rename(method = task_method) |> 
#   dplyr::filter(year >= 2020) |> 
#   st_as_sf(coords=c("Easting", "Northing"), crs=3400, remove=FALSE) |> 
#   st_transform(raster::crs(aoi)) |> 
#   st_intersection(aoi)
# 
# ## 4.2 Separate into bird and covs objects ----
# covs_new <- new_aoi |> 
#   st_drop_geometry() |> 
#   dplyr::select(organization, project_id, surveyid, method, Easting, Northing, year)
# 
# bird_new <- new_aoi |> 
#   st_drop_geometry() |> 
#   dplyr::select(surveyid, ALFL:YRWA)
# 
# ## 4.3 Identify duplicates ----
# loc_new <- covs_new |> 
#   dplyr::select(Easting, Northing, year) |> 
#   unique()
# 
# loc_test <- covs_test |> 
#   dplyr::select(Easting, Northing, year) |> 
#   unique()
# 
# duplicates <- inner_join(loc_new, loc_test)
# 
# ## 4.4. Put together ----
# surveys_test <- covs_test |> 
#   dplyr::select(all_of(colnames(covs_new))) |> 
#   anti_join(duplicates) |> 
#   rbind(covs_new) |> 
#   arrange(surveyid)
# 
# ## 4.5 Filter the bird and offset objects ----
# # Also filter to indicator species
# spp <- sort(c("CONI", "LEFL", "CMWA", "HETH", "OVEN", "PIWO", "BTNW", "NOWA", "CAJA", "RUGR", "YBSA", "DEJU", "BHCO", "CAWA", "OSFL"))
# 
# bird_test <- bird |> 
#   dplyr::filter(surveyid %in% surveys_test$surveyid) |> 
#   dplyr::select(all_of(c("surveyid", spp))) |> 
#   rbind(bird_new  |> 
#           dplyr::filter(surveyid %in% surveys_test$surveyid) |> 
#           dplyr::select(all_of(c("surveyid", spp)))) |> 
#   arrange(surveyid)
# 
# off_test <- off |> 
#   dplyr::filter(surveyid %in% surveys_test$surveyid) |> 
#   dplyr::select(all_of(c("surveyid", spp))) |> 
#   rbind(new_off  |> 
#           dplyr::filter(surveyid %in% surveys_test$surveyid) |> 
#           dplyr::select(all_of(c("surveyid", spp)))) |> 
#   arrange(surveyid)

#5. BADR attribution ----

## 5.1 Make an sf with 150 m buffer -----
# get the area and transform to the badr object crs
covs_buff <- surveys_test |> 
  st_as_sf(coords = c("Easting", "Northing"), crs=3400, remove=FALSE) |> 
  st_buffer(150) |> 
  mutate(area = round(as.numeric(st_area(geometry)), 1)) |> 
  st_transform(raster::crs(badr))

## 5.2 Get the lu attributes -----
covs_lu <- st_intersection(covs_buff, lu)

## 5.3 Get the BADR treatment proportions ----
set.seed(1234)
covs_test <- covs_lu |> 
# sample_n(10) |> 
  st_intersection(badr) |> 
  mutate(tmntarea = as.numeric(st_area(geometry))/area) |> 
  st_drop_geometry() |> 
  pivot_wider(names_from=Treatment, values_from=tmntarea, values_fn=sum, values_fill=0)
  
#6. Save ----
save(covs_train, bird_train, off_train, boot, file=file.path(rootout, "Deviation From Expected", "Data", "Train.Rdata"))

save(covs_test, bird_test, off_test, file=file.path(rootout, "Deviation From Expected", "Data", "Test.Rdata"))
