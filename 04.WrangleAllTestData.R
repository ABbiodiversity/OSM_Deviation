# ---
# title: "04.WrangleAllTestData"
# author: "Elly Knight"
# created: "2025-03-04"
# inputs: wrangled new data, bird model dataset, gis objects
# outputs: test dataset for model prediction and testing hypotheses
# notes:

# ---

# 1. Setup ----

## 1.1 Load packages ----
library(tidyverse) # data manipulation and visualization
library(sf) # gis
library(wildrtrax) # qpad corrections
library(terra) # raster handling for qpad corrections
library(QPAD)

## 1.2 Set GD roots----
rootin <- "G:/Shared drives/ABMI_ECKnight/Projects/BirdModels"
rootout <- "G:/Shared drives/ABMI_ECKnight/Projects/OSM"

## 1.3 Import bird model data ----
load(file.path(rootin, "Data", "Harmonized.Rdata"))
load(file.path(rootin, "Data", "Stratified.Rdata"))

## 1.4 Load the additioanl downloaded data ----
load(file.path(rootout, "Deviation From Expected", "Data", "NewWrangledData.Rdata"))

## 1.5 Import LU shapefile ----
lu <- read_sf(file.path(rootout, "GIS", "OSR_LU_fxd", "OSR_LUs_fxd.shp"))

## 1.6 Import BADR treatment ----
# badr <- read_sf(file.path(rootout, "GIS", "year_2021_treat.shp")) |>
#   st_make_valid()

#2. Data combination ----

## 2.1 Adjust surveyids in new data ----
covs_new$surveyid <- covs_new$surveyid + max(covs$surveyid)
bird_new$surveyid <- bird_new$surveyid + max(covs$surveyid)
off_new$surveyid <- off_new$surveyid + max(covs$surveyid) 

## 2.2 Add task duration to bird model data ----
#we'll need this to get the QPAD corrections
covs_dur <- covs |> 
  left_join(use |> 
              dplyr::select(project_id, gisid, date_time, duration),
            multiple="all") |> 
  rename(task_duration = duration)

## 2.2 Wrangle bird model data ----
covs_mod <- covs_dur |> 
  mutate(year = year + 1992) |> 
  dplyr::select(all_of(colnames(covs_new)))

## 2.2 Identify duplicates ----
loc_new <- covs_new |>
  dplyr::select(Easting, Northing, year) |>
  unique()

loc_mod <- covs_mod |>
  dplyr::select(Easting, Northing, year) |>
  unique()

duplicates <- inner_join(loc_new, loc_mod)

## 2.3 Put covs together ----
covs_all <- covs_mod |> 
  anti_join(duplicates) |> 
  rbind(covs_new)

#3. Data preparation ----

## 3.1 Merge LUs to single AOI ----

aoi <- st_union(lu)

## 3.2 Get unique locations * year ----
loc <- covs_all |> 
  dplyr::select(Easting, Northing) |> 
  unique()

## 3.3 Make it an sf ----
loc_sf <- loc |> 
  st_as_sf(coords = c("Easting", "Northing"), crs=3400, remove = FALSE) |> 
  st_transform(raster::crs(aoi))

#4. Data filtering ----

## 4.1 Get the locations within the aoi ----
loc_aoi <- loc_sf |> 
  st_intersection(aoi)

## 4.2 Filter the covs object to aoi ----
covs_aoi <- inner_join(covs_all, loc_aoi |> st_drop_geometry())

## 4.3 Split out test data based on year ----
covs_test <- covs_aoi |> 
  dplyr::filter(year >= 2020)

## 4.4 Compile the other objects -----

# Also filter to indicator species
spp <- sort(c("CONI", "LEFL", "CMWA", "HETH", "OVEN", "PIWO", "BTNW", "NOWA", "CAJA", "RUGR", "YBSA", "DEJU", "BHCO", "CAWA", "OSFL"))

bird_test <- bird |> 
  dplyr::select(c("surveyid", all_of(spp))) |> 
  rbind(bird_new |> dplyr::select(c("surveyid", all_of(spp)))) |> 
  dplyr::filter(surveyid %in% covs_test$surveyid)

off_test <- off |> 
  dplyr::select(c("surveyid", all_of(spp))) |> 
  rbind(off_new |> dplyr::select(c("surveyid", all_of(spp)))) |> 
  dplyr::filter(surveyid %in% covs_test$surveyid)

#5. Get the density corrections ----

## 5.1 Load adapted wildrtrax functions ----
source("00.QPADCorrections.R")

## 5.2 Add lat lon to data ----
covs_latlon <- covs_test |> 
  st_as_sf(coords = c("Easting", "Northing"), remove=FALSE, crs=3400) |> 
  st_transform(crs=4326) |> 
  st_coordinates() |> 
  data.frame() |> 
  cbind(covs_test) |> 
  rename(longitude = X, latitude = Y)

## 5.4 Get corrections ----
corr_test <- qpad_correction(covs_latlon,
                             species = c("LEFL", "CMWA", "HETH", "OVEN", "PIWO", "BTNW", "NOWA", "GRAJ", "RUGR", "YBSA", "DEJU", "BHCO", "CAWA", "OSFL")) |> 
  rename(CAJA = GRAJ) |> 
  rowwise() |> 
  mutate(CONI = mean(c_across(everything()))) |> 
  ungroup()

#6. BADR attribution ----

# 6.1 Make an sf ----
covs_sf <- covs_test |> 
  st_as_sf(coords = c("Easting", "Northing"), crs=3400, remove=FALSE) |> 
  st_transform(raster::crs(lu))

## 6.1 Make with 150 m buffer -----
# get the area and transform to the badr object crs
covs_buff <- covs_test |> 
  st_as_sf(coords = c("Easting", "Northing"), crs=3400, remove=FALSE) |> 
  st_buffer(150) |> 
  mutate(area = round(as.numeric(st_area(geometry)), 1)) |> 
  st_transform(raster::crs(lu))

## 6.2 Get the lu attributes -----
# there's 2 that are in 2 lus for some reason, so get 1 per surveyid
set.seed(1234)
covs_lu <- st_intersection(covs_sf, lu) |> 
  st_drop_geometry() |> 
  group_by(surveyid) |> 
  sample_n(1) |> 
  ungroup()

## 6.3 Get the BADR treatment proportions ----
covs_tmnt <- covs_buff |> 
  st_intersection(badr) |> 
  mutate(tmntarea = as.numeric(st_area(geometry))/area) |> 
  st_drop_geometry() |> 
  pivot_wider(names_from=Treatment, values_from=tmntarea, values_fn=sum, values_fill=0)

## 6.4 Put together ----
covs_badr <- inner_join(covs_lu, covs_tmnt)

#7. Save -----
save(covs_test, bird_test, off_test, corr_test, covs_badr, file=file.path(rootout, "Deviation From Expected", "Data", "Test.Rdata"))
