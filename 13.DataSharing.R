
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

wt_proj_names <- wt_get_projects('ARU')

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


##################################################
# Add metadata for locations used in modeling.
##################################################

#Process will be: read in main report.
#Get project_id, location_id, year.
#use those to get gisid.
#Use gisid to get covariates.
#Download project report to get project name.
#Combine.

#To run this part of the script alone, first source the top of the script up to 
#where wt_proj_names is defined.

#First 2021-2024 data.
main <- read.csv(file.path(root, 'Data', 'Data_shared_2026-04-28', 'ABMI OSM 2021-2024 ARU Main Reports.csv'))

#Also read train and test data and keep only needed objects.
load(file.path(root, 'Data', 'Train.RData'))
load(file.path(root, 'Data', 'Test.RData'))
rm(list = c('bird_test', 'bird_train', 'boot', 'corr_test', 'off_test', 'off_train'))

#Filter columns.
main <- main |>
  select(project_id, location, location_id, recording_date_time)

#Year.
main$year <- as.numeric(substr(main$recording_date_time, 1, 4))

#distinct.
main <- main |>
  select(-recording_date_time) |>
  distinct()

#gisid
main$gisid <- paste(main$location_id, main$year, sep = '_')

#Check how many match.
sum(main$gisid %in% covs_test$gisid)

#Second attempt at gisid.
main$gisid <- ifelse(main$gisid %in% covs_test$gisid, 
                     main$gisid, 
                     paste(main$location, main$project_id, main$year, sep = '_'))

#Check how many match.
sum(main$gisid %in% covs_test$gisid) #677 out of 888.

#Of the remaining ~200 points, most are 2024.
#After discussion with Elly, we agreed that a good approach is to share
#the covariates where they are available. For the other points, they were
#not included in the analysis, so these covariates were not extracted.

#Covariate names.
covariate_names <- c("vegc", "soilc", "vegw", "soilw", 
                     "wtAge", "wtAge2", "wtAge05", "isCon", "isUpCon", "isBogFen", 
                     "isMix", "isPine", "isWSpruce", "fcc2", "road", "mWell", "mSoft", 
                     "mEnSft", "mTrSft", "mSeism", "pWater_KM", "pWater2_KM", 
                     "paspen", "MAP", "TD", "CMD", "FFP", "EMT")

#Extract covariate columns.
covs_test_unique <- unique(covs_test[,c('gisid', covariate_names)])

#Note that some duplicates exist related to road, pwater and pwater2 variables having
#different values for each surveyid within a year.
covs_view <- aggregate(. ~ gisid, data = covs_test_unique, FUN=n_distinct)
apply(covs_view, 2, function(x) length(unique(x))) 

#Split out problem variables, calculate medians.
covs_test_char <- unique(covs_test_unique[,!colnames(covs_test_unique) %in% c('road', "pWater_KM", "pWater2_KM")])
covs_test_median <- aggregate(. ~ gisid, data = covs_test_unique[,c('gisid', 'road', "pWater_KM", "pWater2_KM")], FUN=median)

#Add to main object.
main <- left_join(main, covs_test_char)
main <- left_join(main, covs_test_median)

#Check that the only NAs are in the 211 sites that didn't have covariates.
#Note there are an addition 13 NAs in the soilw and soilc variables.
apply(main, 2, function(x) sum(is.na(x)))

#Joint to add project.
main <- left_join(main, wt_proj_names[,c('project_id', 'project')])

#Only for the 2021-2024 data, add the badr variables.
dput(colnames(covs_badr)[!colnames(covs_badr) %in% colnames(main)])

#badr colnames
badr_column_names <- c("ClusterB", "COUNT_HUC1", "SUM_hapark", 
                       "SUM_hamine", "SUM_hapipe", "SUM_haroad", "SUM_haseis", "SUM_hawell", 
                       "SUM_hawe_1", "SUM_hawe_2", "SUM_OSMdat", "propmine", "proppipe", 
                       "proproad", "propseismi", "propallwel", "cei", "propdecid", "propmixed", 
                       "propcrop", "propupland", "propwater", "fireareaha", "insituarea", 
                       "Shape_Leng", "propfire", "propinsitu", "LanduseReg", "Selected", 
                       "Avg_TDN", "Max_TDN", "SD_TDN", "Avg_TDS", "Max_TDS", "SD_TDS", 
                       "deciles", "High Activity Insitu Well Pads", 
                       "Roads", "Plant/Mine", "Dense Linear Features", "Low Activity Well Pads", 
                       "Plant/Mine Buffer", "Low Disturbance/Reference")

main <- left_join(main, covs_badr[,c()])


#Historical data ----------------------

#Now the historical data.
historic <- read.csv(file.path(root, 'Data', 'Data_shared_2026-04-28', 'ABMI Historic ARU Main Reports.csv'))

#Filter columns.
historic <- historic |>
  select(project_id, location, location_id, recording_date_time)

#Year.
historic$year <- as.numeric(substr(historic$recording_date_time, 1, 4))

#distinct.
historic <- historic |>
  select(-recording_date_time) |>
  distinct()

#gisid
historic$gisid <- paste(historic$location, historic$project_id, historic$year, sep = '_')

#Check how many match.
sum(historic$gisid %in% covs_train$gisid) #1471/1754 are in train.
sum(historic$gisid %in% c(covs_train$gisid, covs_test$gisid)) #1565 in train/test.
sum(historic$gisid %in% c(covs_train$gisid, covs_test$gisid) | historic$location_id %in% covs_train$locationid) #1579 when location_id is added.

#Extract covariate columns.
covs_unique <- unique(rbind(covs_test[,c('gisid', covariate_names)],
                            covs_train[,c('gisid', covariate_names)]))

#Note that some duplicates exist related to road, pwater and pwater2 variables having
#different values for each surveyid within a year.
covs_view <- aggregate(. ~ gisid, data = covs_unique, FUN=n_distinct)
apply(covs_view, 2, function(x) length(unique(x)))

#Split out problem variables, calculate medians.
covs_char <- unique(covs_unique[,!colnames(covs_unique) %in% c('road', "pWater_KM", "pWater2_KM")])
covs_median <- aggregate(. ~ gisid, data = covs_unique[,c('gisid', 'road', "pWater_KM", "pWater2_KM")], FUN=median)

#Add to historic object.
historic <- left_join(historic, covs_char)
historic <- left_join(historic, covs_median)

#Check that the only NAs are in the 189 sites that didn't have covariates.
#Note there are an addition 50 NAs in the soilw and soilc variables.
apply(historic, 2, function(x) sum(is.na(x)))

#Joint to add project.
historic <- left_join(historic, wt_proj_names[,c('project_id', 'project')])

#Riverforks data ----------------------

#Same procedure for the riverforks data.
rf <- read.csv(file.path(root, 'Data', 'Data_shared_2026-04-28', 'ABMI Historic Riverforks Main Report.csv'))
rf <- unique(rf[,c('SITE', 'TBB_POINT_COUNT', 'YEAR')])
rf$location <- paste0(rf$SITE, '-', rf$TBB_POINT_COUNT)

#Filter columns. Join to get year, 
rf_meta <- data.frame(project_id = 9998, location = sort(unique(rf$location)),
                      location_id = NA) |>
  left_join(unique(rf[,c('location', 'YEAR')])) |>
  rename(year = YEAR) |>
  distinct()

#gisid
rf_meta$gisid <- paste(rf_meta$location, rf_meta$project_id, rf_meta$year, sep = '_')

#Check how many match.
sum(rf_meta$gisid %in% covs_train$gisid) #2718/4726 are in train.
sum(rf_meta$gisid %in% c(covs_train$gisid, covs_test$gisid)) #No additional points in test (as expected).
sum(rf_meta$gisid %in% c(covs_train$gisid, covs_test$gisid) | rf_meta$location_id %in% covs_train$locationid) #None when location_id is added.

#Extract covariate columns.
covs_rf_unique <- unique(covs_train[,c('gisid', covariate_names)])

#Note that some duplicates exist related to road, pwater and pwater2 variables having
#different values for each surveyid within a year.
covs_rf_view <- aggregate(. ~ gisid, data = covs_unique, FUN=n_distinct)
apply(covs_rf_view, 2, function(x) length(unique(x)))

#Split out problem variables, calculate medians.
covs_rf_char <- unique(covs_rf_unique[,!colnames(covs_rf_unique) %in% c('road', "pWater_KM", "pWater2_KM")])
covs_rf_median <- aggregate(. ~ gisid, data = covs_rf_unique[,c('gisid', 'road', "pWater_KM", "pWater2_KM")], FUN=median)

#Add to rf_meta object.
rf_meta <- left_join(rf_meta, covs_rf_char)
rf_meta <- left_join(rf_meta, covs_rf_median)

#Check that the only NAs are in the 2008 sites that didn't have covariates.
#Note there are an addition 309 NAs in the soilw and soilc variables.
apply(rf_meta, 2, function(x) sum(is.na(x)))

#Joint to add project.
rf_meta$project <- 'Riverforks Historical Recordings'

#Write separate metadata files -----------------------------------------

#Re-order columns. Also removing gisid.
col_order <- c("project", "project_id", "location", "location_id", "year", "vegc", 
               "soilc", "vegw", "soilw", "wtAge", "wtAge2", "wtAge05", "isCon", 
               "isUpCon", "isBogFen", "isMix", "isPine", "isWSpruce", "fcc2", 
               "mWell", "mSoft", "mEnSft", "mTrSft", "mSeism", "paspen", "MAP", 
               "TD", "CMD", "FFP", "EMT", "road", "pWater_KM", "pWater2_KM")

main <- main[,col_order]
historic <- historic[,col_order]
rf_meta <- rf_meta[,col_order]

write.csv(main, file.path(root, 'Data', 'Data_shared_2026-04-28', 'ABMI OSM 2021-2024 ARU Deployment Metadata.csv'),
          row.names = F)
write.csv(historic, file.path(root, 'Data', 'Data_shared_2026-04-28', 'ABMI Historic ARU Deployment Metadata.csv'),
          row.names = F)
write.csv(rf_meta, file.path(root, 'Data', 'Data_shared_2026-04-28', 'ABMI Historic Riverforks Deployment Metadata.csv'),
          row.names = F)
          




