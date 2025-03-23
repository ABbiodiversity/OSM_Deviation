# ---
# title: "01.WrangleTrainData"
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

#2. Data preparation ----

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

#3. Data filtering ----

## 3.1 Get the locations within the aoi ----
loc_aoi <- loc_sf |> 
  st_intersection(aoi)

## 3.2 Filter the covs object to aoi ----
covs_aoi <- covs |> 
  dplyr::filter(locationid %in% loc_aoi$locationid)

## 3.3 Split out training data based on year ----
#also take out eBird because of qpad bug
covs_train <- covs_aoi |> 
  mutate(year = year + 1992) |> 
  dplyr::filter(year < 2020, year >= 2010,
                organization!="eBird")

## 3.4 Filter the other objects -----

bird_train <- bird |> 
  dplyr::filter(surveyid %in% covs_train$surveyid)

off_train <- off |> 
  dplyr::filter(surveyid %in% covs_train$surveyid)

#4. Save ----
save(covs_train, bird_train, off_train, boot, file=file.path(rootout, "Deviation From Expected", "Data", "Train.Rdata"))
