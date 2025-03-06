#1. Make X ----

.make_x <- function(data, tz="local", check_xy=TRUE) {
  
  # Download message
  message("Downloading geospatial assets. This may take a moment.")
  
  # Function to download and read a raster file using httr2
  download_and_read_raster <- function(url, filename) {
    req <- httr2::request(url) %>%
      httr2::req_perform()  # Perform the request
    
    # Save the response content to a file
    writeBin(req$body, filename)
    
    return(terra::rast(filename))  # Read the raster file
  }
  
  # Download and read TIFF files
  .rlcc <- download_and_read_raster("https://raw.githubusercontent.com/ABbiodiversity/wildRtrax-assets/main/lcc.tif", "lcc.tif")
  .rtree <- download_and_read_raster("https://raw.githubusercontent.com/ABbiodiversity/wildRtrax-assets/main/tree.tif", "tree.tif")
  .rd1 <- download_and_read_raster("https://raw.githubusercontent.com/ABbiodiversity/wildRtrax-assets/main/seedgrow.tif", "seedgrow.tif")
  .rtz <- download_and_read_raster("https://raw.githubusercontent.com/ABbiodiversity/wildRtrax-assets/main/utcoffset.tif", "utcoffset.tif")
  
  crs <- terra::crs(.rtree)
  
  #get vars
  date <- date(data$date_time)
  time <- substr(as.character(data$date_time), 12, 19)
  lon <- as.numeric(data$longitude)
  lat <- as.numeric(data$latitude)
  dur <- as.numeric(data$task_duration)
  dis <- Inf
  
  #parse date+time into POSIXlt
  if(tz=="local"){
    dtm <- strptime(paste0(date, " ", time, ":00"),
                    format="%Y-%m-%d %H:%M:%S", tz="America/Edmonton")
  }
  if(tz=="utc"){
    dtm <- strptime(paste0(date, " ", time, ":00"),
                    format="%Y-%m-%d %H:%M:%S", tz="GMT")
  }
  day <- as.integer(dtm$yday)
  hour <- as.numeric(round(dtm$hour + dtm$min/60, 2))
  
  #checks
  checkfun <- function(x, name="", range=c(-Inf, Inf)) {
    if (any(x[!is.na(x)] < range[1] | x[!is.na(x)] > range[2])) {
      stop(sprintf("Parameter %s is out of range [%.0f, %.0f]", name, range[1], range[2]))
    }
    invisible(NULL)
  }
  #Coordinates
  if (check_xy) {
    checkfun(lon, "lon", c(-164, -52))
    checkfun(lat, "lat", c(39, 69))
  }
  if (any(is.infinite(lon)))
    stop("Parameter lon must be finite")
  if (any(is.infinite(lat)))
    stop("Parameter lat must be finite")
  
  #handling missing values
  ok_xy <- !is.na(lon) & !is.na(lat)
  #Other fields
  checkfun(day, "day", c(0, 365))
  checkfun(hour, "hour", c(0, 24))
  checkfun(dur, "dur", c(0, Inf))
  
  #intersect here
  xydf <- data.frame(x=lon, y=lat)
  xydf$x[is.na(xydf$x)] <- mean(xydf$x, na.rm=TRUE)
  xydf$y[is.na(xydf$y)] <- mean(xydf$y, na.rm=TRUE)
  xy <- vect(xydf, geom=c("x", "y"), crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  xy <- project(xy, crs)
  
  #LCC4 and LCC2
  vlcc <- terra::extract(.rlcc, xy)$lcc
  lcclevs <- c("0"="", "1"="Conif", "2"="Conif", "3"="", "4"="",
               "5"="DecidMixed", "6"="DecidMixed", "7"="", "8"="Open", "9"="",
               "10"="Open", "11"="Open", "12"="Open", "13"="Open", "14"="Wet",
               "15"="Open", "16"="Open", "17"="Open", "18"="", "19"="")
  lcc4 <- factor(lcclevs[vlcc+1], c("DecidMixed", "Conif", "Open", "Wet"))
  lcc2 <- lcc4
  levels(lcc2) <- c("Forest", "Forest", "OpenWet", "OpenWet")
  
  #TREE
  vtree <- terra::extract(.rtree, xy)$tree
  TREE <- vtree / 100
  TREE[TREE < 0 | TREE > 1] <- 0
  
  #raster::extract seedgrow value (this is rounded)
  d1 <- terra::extract(.rd1, xy)$seedgrow
  
  #UTC offset + 7 makes Alberta 0 (MDT offset) for local times
  if(tz=="local"){
    ltz <- terra::extract(.rtz, xy)$utcoffset + 7
  }
  if(tz=="utc"){
    ltz <- 0
  }
  
  message("Removing geospatial assets from local")
  
  # Remove once downloaded and read
  file.remove(list.files(pattern = "*.tif$"))
  
  #sunrise time adjusted by offset
  ok_dt <- !is.na(dtm)
  dtm[is.na(dtm)] <- mean(dtm, na.rm=TRUE)
  if(tz=="local"){
    sr <- suntools::sunriset(cbind("X"=xydf$x, "Y"=xydf$y),
                             as.POSIXct(dtm, tz="America/Edmonton"),
                             direction="sunrise", POSIXct.out=FALSE) * 24
  }
  if(tz=="utc"){
    sr <- suntools::sunriset(cbind("X"=xydf$x, "Y"=xydf$y),
                             as.POSIXct(dtm, tz="GMT"),
                             direction="sunrise", POSIXct.out=FALSE) * 24
  }
  TSSR <- round(unname((hour - sr + ltz) / 24), 4)
  
  #days since local spring
  DSLS <- (day - d1) / 365
  
  #transform the rest
  JDAY <- round(day / 365, 4) # 0-365
  TREE <- round(vtree / 100, 4)
  MAXDIS <- round(dis / 100, 4)
  MAXDUR <- round(dur, 4)
  
  out <- data.frame(
    TSSR=TSSR,
    JDAY=JDAY,
    DSLS=DSLS,
    LCC2=lcc2,
    LCC4=lcc4,
    TREE=TREE,
    MAXDUR=MAXDUR,
    MAXDIS=MAXDIS)
  out$TSSR[!ok_xy | !ok_dt] <- NA
  out$DSLS[!ok_xy] <- NA
  out$LCC2[!ok_xy] <- NA
  out$LCC4[!ok_xy] <- NA
  out$TREE[!ok_xy] <- NA
  
  return(out)
  
}

#2. Make offsets ----

.make_off <- function(spp, x){
  
  if (length(spp) > 1L)
    stop("spp argument must be length 1. Use a loop or map for multiple species.")
  spp <- as.character(spp)
  
  #checks
  if (!(spp %in% getBAMspecieslist()))
    stop(sprintf("Species %s has no QPAD estimate available", spp))
  
  #constant for NA cases
  cf0 <- exp(unlist(coefBAMspecies(spp, 0, 0)))
  
  #best model
  mi <- bestmodelBAMspecies(spp, type="BIC")
  cfi <- coefBAMspecies(spp, mi$sra, mi$edr)
  
  TSSR <- x$TSSR
  DSLS <- x$DSLS
  JDAY <- x$JDAY
  lcc2 <- x$LCC2
  lcc4 <- x$LCC4
  TREE <- x$TREE
  MAXDUR <- x$MAXDUR
  MAXDIS <- x$MAXDIS
  n <- nrow(x)
  
  #Design matrices for singing rates (`Xp`) and for EDR (`Xq`)
  Xp <- cbind(
    "(Intercept)"=1,
    "TSSR"=TSSR,
    "JDAY"=JDAY,
    "TSSR2"=TSSR^2,
    "JDAY2"=JDAY^2,
    "DSLS"=DSLS,
    "DSLS2"=DSLS^2)
  
  Xq <- cbind("(Intercept)"=1,
              "TREE"=TREE,
              "LCC2OpenWet"=ifelse(lcc4 %in% c("Open", "Wet"), 1, 0),
              "LCC4Conif"=ifelse(lcc4=="Conif", 1, 0),
              "LCC4Open"=ifelse(lcc4=="Open", 1, 0),
              "LCC4Wet"=ifelse(lcc4=="Wet", 1, 0))
  
  p <- rep(NA, n)
  A <- q <- p
  
  #design matrices matching the coefs
  Xp2 <- Xp[,names(cfi$sra),drop=FALSE]
  OKp <- rowSums(is.na(Xp2)) == 0
  Xq2 <- Xq[,names(cfi$edr),drop=FALSE]
  OKq <- rowSums(is.na(Xq2)) == 0
  
  #calculate p, q, and A based on constant phi and tau for the respective NAs
  p[!OKp] <- sra_fun(MAXDUR[!OKp], cf0[1])
  unlim <- ifelse(MAXDIS[!OKq] == Inf, TRUE, FALSE)
  A[!OKq] <- ifelse(unlim, pi * cf0[2]^2, pi * MAXDIS[!OKq]^2)
  q[!OKq] <- ifelse(unlim, 1, edr_fun(MAXDIS[!OKq], cf0[2]))
  
  #calculate time/lcc varying phi and tau for non-NA cases
  phi1 <- exp(drop(Xp2[OKp,,drop=FALSE] %*% cfi$sra))
  tau1 <- exp(drop(Xq2[OKq,,drop=FALSE] %*% cfi$edr))
  p[OKp] <- sra_fun(MAXDUR[OKp], phi1)
  unlim <- ifelse(MAXDIS[OKq] == Inf, TRUE, FALSE)
  A[OKq] <- ifelse(unlim, pi * tau1^2, pi * MAXDIS[OKq]^2)
  q[OKq] <- ifelse(unlim, 1, edr_fun(MAXDIS[OKq], tau1))
  
  #log(0) is not a good thing, apply constant instead
  ii <- which(p == 0)
  p[ii] <- sra_fun(MAXDUR[ii], cf0[1])
  
  #package output
  data.frame(
    p=p,
    q=q,
    A=A,
    correction=p*A*q,
    offset=log(p) + log(A) + log(q))
  
}

#3. Wrapper ----

qpad_correction <- function (data, species = c("all"), version = 3, together = FALSE) 
{
  if ("survey_url" %in% colnames(data)) {
    data <- ungroup(mutate(rowwise(rename(data, task_id = survey_id, 
                                          recording_date_time = survey_date, observer_id = observer)), 
                           durationMethod = ifelse(substr(survey_duration_method, 
                                                          nchar(survey_duration_method), nchar(survey_duration_method)) == 
                                                     "+", substr(survey_duration_method, 1, nchar(survey_duration_method) - 
                                                                   2), survey_duration_method), chardur = gregexpr("-", 
                                                                                                                   durationMethod, fixed = TRUE), chardurmax = max(unlist(chardur)), 
                           task_duration = as.numeric(substr(durationMethod, 
                                                             chardurmax + 1, nchar(durationMethod) - 3)) * 
                             60, chardis = gregexpr("-", survey_distance_method, 
                                                    fixed = TRUE), chardismax = max(unlist(chardis)), 
                           distance1 = substr(survey_distance_method, chardismax + 
                                                1, nchar(survey_distance_method) - 1), task_distance = ifelse(distance1 %in% 
                                                                                                                c("AR", "IN"), Inf, as.numeric(distance1))))
  }
  cat("Extracting covariates for offset calculation. This may take a moment.")
  x <- .make_x(data)
  cat("\nLoading QPAD estimates... ")
  load_BAM_QPAD(version)
  if ("all" %in% species) 
    spp <- sort(intersect(getBAMspecieslist(), colnames(data)))
  else spp <- species
  cat("\nCalculating offsets...")
  off <- matrix(0, nrow(x), length(spp))
  colnames(off) <- spp
  for (i in 1:length(spp)) {
    cat("\n", spp[i])
    o <- .make_off(spp[i], x)
    off[, i] <- o$correction
  }
  if (together == FALSE) {
    return(data.frame(off))
  }
  if (together == TRUE) {
    out <- cbind(data, rename_with(data.frame(off), .fn = ~paste0(.x, 
                                                                  ".off")))
    if ("survey_url" %in% colnames(data)) {
      out <- dplyr::select(rename(out, survey_id = task_id, 
                                  survey_date = recording_date_time, observer = observer_id), 
                           -durationMethod, -chardur, -chardurmax, -task_duration, 
                           -chardis, -chardismax, -distance1, -task_distance)
    }
    return(out)
  }
  cat("\nDone!")
}
