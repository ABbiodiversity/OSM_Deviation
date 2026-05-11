
# ---
# title: "13. Data sharing"
# author: "Richard Hedley"
# created: "2026-04-28"
# inputs: "Train.RData and Test.RData"
# outputs: "csv files"
# notes:

# ---

# 1. Setup ----

## 1.1 Load packages
library(tidyverse) # data manipulation and visualization
library(wildrtrax) # data download
library(sf)

## 1.2 Set GD roots
root <- "G:/Shared drives/ABMI_RHedley/Projects/OSM/Deviation From Expected"

## 1.3 Login to WildTrax
config <- "00.WTlogin.R"
source(config)

## 1.4 Authenticate
wt_auth()

# 2. Download ----

## 2.1 List of projects
projects <- c(686, 1174, 2088, 3444)
names(projects) <- c(2021:2024)

## 2.2 Download main reports
dat.list <- list()
for(i in 1:length(projects)){
  
  dat.list[[i]] <- wt_download_report(projects[i], sensor="ARU", report = "main")
  
  print(projects[i])
}

rec.list <- list()
for(i in 1:length(projects)){
  
  rec.list[[i]] <- wt_download_report(projects[i], sensor="ARU", report = "recording")
  
  print(projects[i])
}

## 2.3 Collapse list

dat <- bind_rows(dat.list)
rec <- bind_rows(rec.list)

## 2.4 Write to csv.

write.csv(dat, file.path(root, 'Data', 'Data_shared_2026-04-28', 'ABMI OSM 2021-2024 ARU Main Reports.csv'), row.names = FALSE)
write.csv(rec, file.path(root, 'Data', 'Data_shared_2026-04-28', 'ABMI OSM 2021-2024 ARU Recording Reports.csv'), row.names = FALSE)

rm(list = c('dat', 'rec', 'rec.list', 'dat.list', 'projects'))


# 3. Collate training data.----

load(file.path(root, 'Data', 'Train.RData'))

#Filter to organization = ABMI.
covs_train <- covs_train[covs_train$organization == 'ABMI',]

#projects.
train_projects <- sort(unique(covs_train$project_id))

#Remove RF.
train_projects <- train_projects[train_projects != 9998]

## Download main reports
dat.list <- list()
for(i in 1:length(train_projects)){
  
  dat.list[[i]] <- wt_download_report(train_projects[i], sensor="ARU", report = "main")
  
  print(train_projects[i])
}

rec.list <- list()
for(i in 1:length(train_projects)){
  
  rec.list[[i]] <- wt_download_report(train_projects[i], sensor="ARU", report = "recording")
  
  print(train_projects[i])
}

dat_train <- bind_rows(dat.list)
rec_train <- bind_rows(rec.list)

# Filter to lu polygon.
lu <- read_sf(file.path(dirname(root), "GIS", "OSR_LU_fxd", "OSR_LUs_fxd.shp"))

## Merge LUs to single AOI 

aoi <- st_union(lu)

## Make it an sf 
dat_train_sf <- dat_train |> 
  filter(!is.na(longitude)) |>
  st_as_sf(coords = c("longitude", "latitude"), crs=4326, remove = FALSE) |> 
  st_transform(st_crs(aoi))

# Get the locations within the aoi 
dat_train_aoi <- dat_train_sf |> 
  st_filter(aoi) 

#Visualize.
plot(st_geometry(dat_train_sf))
plot(st_geometry(aoi), border = 'red', add = TRUE)
plot(st_geometry(dat_train_aoi), add = TRUE, col = 'orange')

#Drop geometry.
dat_train_aoi <- st_drop_geometry(dat_train_aoi)

#Filter rec_train using recording_id.
rec_train_filter <- rec_train[rec_train$recording_id %in% dat_train_aoi$recording_id,]

write.csv(dat_train_aoi, file.path(root, 'Data', 'Data_shared_2026-04-28', 'ABMI Historic ARU Main Reports.csv'), row.names = FALSE)
write.csv(rec_train_filter, file.path(root, 'Data', 'Data_shared_2026-04-28', 'ABMI Historic ARU Recording Reports.csv'), row.names = FALSE)

#4. Filter Riverforks main report.

rf <- read.csv("G:/Shared drives/ABMI_RHedley/Projects/BirdModels/Data/Riverforks/A_RT_SITE_PHYCHAR_202301161554.csv")
rf_main <- read.csv("G:/Shared drives/ABMI_RHedley/Projects/BirdModels/Data/Riverforks/RT_BIRD_COUNT_BL.csv")

## Make it an sf 
rf_sf <- rf |> 
  filter(!is.na(SITE_LONGITUDE)) |>
  st_as_sf(coords = c("SITE_LONGITUDE", "SITE_LATITUDE"), crs=4326, remove = FALSE) |> 
  st_transform(st_crs(aoi))

# Get the locations within the aoi 
rf_aoi <- rf_sf |> 
  st_filter(aoi) 

#Visualize.
plot(st_geometry(rf_sf))
plot(st_geometry(aoi), border = 'red', add = TRUE)
plot(st_geometry(rf_aoi), add = TRUE, col = 'orange')

#Location column, note they use different column names.
rf_aoi$location <- paste0(rf_aoi$SITE, '-', rf_aoi$TSFG_POINT_COUNT)
rf_main$location <- paste0(rf_main$SITE, '-', rf_main$TBB_POINT_COUNT)

#Filter rf_main using location.
rf_main_filter <- rf_main[rf_main$location %in% rf_aoi$location,]

#Do the reverse, since there are some inconsistencies.
rf_aoi <- rf_aoi[rf_aoi$location %in% rf_main$location,]

#Check.
setequal(rf_main_filter$location, rf_aoi$location)

#Remove location column.
rf_main_filter <- select(rf_main_filter, -location)
rf_aoi <- select(rf_aoi, -location)

#Visualize.
plot(st_geometry(rf_aoi), add = T, col = 'blue')

#Drop geometry.
rf_aoi <- st_drop_geometry(rf_aoi)

#Write to csv.
write.csv(rf_aoi, file.path(root, 'Data', 'Data_shared_2026-04-28', 'ABMI Historic Riverforks Location Report.csv'), row.names = FALSE)
write.csv(rf_main_filter, file.path(root, 'Data', 'Data_shared_2026-04-28', 'ABMI Historic Riverforks Main Report.csv'), row.names = FALSE)





