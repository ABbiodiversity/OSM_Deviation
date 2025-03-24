# ---
# title: "11. Package habitat suitability coefficients"
# author: "Elly Knight"
# created: "2025-03-22"
# inputs: "selected models"
# outputs: "dataframe of standardized coefficients"
# notes:

# ---

# 1. Setup ----

## 1.1 Load packages----
library(tidyverse) #basic data wrangling
library(purrr) #functional programming

## 1.2 Set root path for data on google drive----
root <- "G:/Shared drives/ABMI_ECKnight/Projects/OSM/Deviation From Expected"

## 1.3 Restrict scientific notation----
options(scipen = 99999)

## 1.4 Get the list of models run----
mods <- data.frame(file = list.files(file.path(root, "Results", "LandCoverModels", "Coefficients"), pattern="*.csv", recursive = TRUE),
                   path = list.files(file.path(root, "Results", "LandCoverModels", "Coefficients"), pattern="*.csv", recursive = TRUE, full.names = TRUE)) |>
  separate(file, into=c("f1", "f2", "species", "bootstrap", "f3"))
             
## 1.5 Load model scripts----
source("00.NorthModels.R")

## 1.6 Function to fix interaction term names----
#make formula term names sorted and predictable, i.e. always A:B instead of B:A
fix_names <- function(x, sep=":") {
  unlist(lapply(x, function(z) {
    paste(sort(strsplit(z, sep)[[1]]), collapse=sep)
  }))
}

## 1.7 Function to get model terms-----
#Formula from Peter's approach
get_terms <- function(mods, type=c("formula", "list"), intercept=TRUE) {
  type <- match.arg(type)
  x <- unlist(lapply(unlist(mods), function(z) as.character(z)[3]))
  #    x <- unname(substr(x, 5, nchar(x)))
  x <- gsub(". + ", "", x, fixed=TRUE)
  x <- unlist(strsplit(x, "+", fixed=TRUE))
  x <- unlist(strsplit(x, "*", fixed=TRUE))
  if (type == "list")
    x <- unlist(strsplit(x, ":", fixed=TRUE))
  x <- sub("^[[:space:]]*(.*?)[[:space:]]*$", "\\1", x, perl=TRUE)
  x <- unique(x)
  if (type == "formula") {
    x <- paste("~", paste(x, collapse=" + ", sep=""))
    if (!intercept)
      x <- paste(x, "- 1")
    x <- as.formula(x)
  }
  x
}

## 1.8 Load veg age lookup----
load(file.path("G:/Shared drives/ABMI_ECKnight/Projects/BirdModels", "Data", "lookups", "Xn-veg-v2024.Rdata"))
colnames(age) <- fix_names(colnames(age))

## 1.9 Data ----
load(file.path(root, "Data", "Train.Rdata"))

# 2. Package landcover coefficients ----

#pack it up pack it in, let me begin

## 2.1 Get list of species----
spp <- unique(mods$species)

## 2.2 Get the full list of potential continuous covariates----
#don't need the categorical ones because they're in every model
names.north <- do.call(rbind, unlist(modelsnorth)) |>
  data.frame() |>
  separate(X3, into=c("X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10", "X11", "X12", "X13", "X14", "X15", "X16", "X17", "X18", "X19", "X20", "X21", "X22", "X23"), sep = " +") |>
  dplyr::select(-X1, -X2, -X3) |>
  pivot_longer(cols=X4:X23, names_to="position", values_to="name") |>
  dplyr::filter(!is.na(name), name!="+") |>
  dplyr::select(-position) |>
  unique() |>
  mutate(name = fix_names(name)) |>
  dplyr::filter(!name %in% c("method", "vegc"))

## 2.3 Get the model matrix----
Xn <- model.matrix(get_terms(modelsnorth), covs_train)
colnames(Xn) <- fix_names(c("Intercept", colnames(Xn)[-1]))

## 2.4 Set up loop----
coef.out <- list()
for(i in 1:length(spp)){
  
  ## 2.5 List of models for that species----
  north.i <- dplyr::filter(mods, species==spp[i])
  
  coef.list <- list()
  for(j in 1:nrow(north.i)){
    
    ## 2.6 Get the raw coefficients-----
    #Add zeros for any covariates that weren't in the best model
    coef.j <- read.csv(north.i$path[j]) |>
      mutate(name = fix_names(name)) |>
      full_join(data.frame(name = names.north$name)) |>
      mutate(value = ifelse(is.na(value), 0, value)) |>
      suppressMessages()
    raw.j <- coef.j$value
    names(raw.j) <- coef.j$name
    
    ## 2.7 Translate to standardized & transformed coefficients for the veg coefficients----
    mu <- drop(age %*% raw.j[colnames(age)])
    lam.j <- exp(drop(age %*% raw.j[colnames(age)]))
    
    ## 2.8 Adjust the linear feature coefficients for competing models----
    #replace others with msoft if it is non-zero
    #because mSoft is competed with the others in the model set
    hf.j <- coef.j |>
      dplyr::filter(name %in% c("mWell", "mSoft", "mEnSft", "mTrSft", "mSeism")) |>
      mutate(value = exp(value)) |>
      pivot_wider(names_from=name, values_from=value) |>
      mutate(mEnSft = ifelse(mSoft!=0 & mEnSft==0, mSoft, mEnSft),
             mTrSft = ifelse(mSoft!=0 & mTrSft==0, mSoft, mTrSft),
             mSeism = ifelse(mSoft!=0 & mSeism==0, mSoft, mSeism)) |>
      pivot_longer(mWell:mSeism, names_to="name", values_to="value") |>
      data.frame()
    
    ## 2.9 Adjust the linear feature and well coefficients----
    
    #Human-modified landcover types that we don't want interfering with adjustment
    hfc <- c("Crop", "Industrial", "Mine", "RoughP", "Rural", "TameP", "Urban")
    
    #Dataframe of variables to adjust
    vars <- data.frame(var = c("mWell", "mEnSft", "mTrSft", "mSeism"))
    
    for(k in 1:nrow(vars)){
      
      #Make a mock dataframe for the variable
      zero.k <- data.frame(var = 0)
      colnames(zero.k) <- vars$var[k]
      
      #Get the rows of interest from the model matrix
      #> 0 proportion of variable, no human-modified landover type, no harvest
      rows.k <- covs_train |>
        mutate(rowid = row_number()) |>
        anti_join(zero.k) |> 
        dplyr::filter(!vegc %in% hfc &
                        fcc2==0) |>
        suppressMessages()
      
      Xn.k <- Xn[rows.k$rowid, colnames(age)]
      
      #Make predictions from those rows using the raw coefficient
      lam.k <- exp(Xn.k %*% raw.j[colnames(Xn.k)])
      
      #Multiply transformed coefficients by those predictions and take the mean
      vars$est[k] <- mean(lam.k * hf.j[hf.j$name==vars$var[k],]$value)
      
      #Add some extra tracking information
      vars$n[k] <- nrow(rows.k)
      vars$original[k] <- exp(raw.j[vars$var[k]])
      
    }
    
    ## 2.10 Cap open habitat values----
    #Get maximum lambda for open habitat types
    lam.open <- max(lam.j[c(names(lam.j)[endsWith(names(lam.j), "R")],
                            "GrassHerb", "Shrub", "GraminoidFen", "Marsh")])
    
    #Get maximum lambda for open human footprint types
    lam.hf <- max(lam.j[c("Industrial", "Rural", "Urban")])
    
    #Overall cap value
    lam.max <- max(lam.open, lam.hf)
    
    #Cap the linear features----
    linear <- vars |>
      rowwise() |>
      mutate(capped = min(est, lam.max)) |>
      ungroup() |>
      mutate(cap_open = lam.open,
             cap_hf = lam.hf,
             cap_total = lam.max,
             species = spp[i],
             boot = j,
             name = c("Wellsites", "EnSoftLin", "TrSoftLin", "EnSeismic"))
    
    linear.j <- linear$capped
    names(linear.j) <- linear$name
    
    ## 2.11 Put together----
    lam.out <- c(Climate = exp(raw.j["climate"]),
                 lam.j[names(lam.j)!="Mine"],
                 linear.j,
                 HardLin = 0,
                 Water = 0,
                 Bare = 0,
                 SnowIce = 0,
                 Mine = 0,
                 MineV = unname(lam.j["Mine"]))
    names(lam.out) <- gsub("Spruce", "WhiteSpruce", names(lam.out))
    names(lam.out) <- gsub("Decid", "Deciduous", names(lam.out))
    names(lam.out) <- gsub("Climate.climate", "Climate", names(lam.out))
    names(lam.out) <- gsub("TreedBog", "BlackSpruce", names(lam.out))
    
    ## 2.12 Transform back and cap values----
    lam.final <- log(lam.out)
    lam.final[lam.final > 10^4] <- 10^4
    lam.final[lam.final < -10^4] <- -10^4
    
    coef.list[[j]] <- lam.final
    
  }
  
  ## 2.13 Add species to array----
  coef.out[[i]] <- data.frame(do.call(rbind, coef.list)) |> 
    mutate(species = spp[i],
           boot = seq(1, 25, 1))
  
  cat("Finished species", i, "of", length(spp), "\n")
  
}

## 2.14 Collapse and save ----
coef <- do.call(rbind, coef.out)

write.csv(coef, file.path(root, "Results", "Coefficients.csv"), row.names = FALSE)
