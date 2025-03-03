# ---
# title: "03.ModelClimate"
# author: "Elly Knight"
# created: "2025-03-03"
# inputs: train data object from `02.WrangleData.R`
# outputs: climate models
# notes: this script is nearly identical to the `06.ModelClimate.R` scirpt in the BirdModels repo. See that script for details
# ---

#PREAMBLE############################

#1. Load packages----
print("* Loading packages on master *")
library(tidyverse) #basic data wrangling
library(AICcmodavg) # For model selection
library(MuMIn) #Model averaging
library(parallel) #parallel computing

#2. Determine if testing and on local or cluster----
test <- FALSE
cc <- FALSE

#3. Set nodes for local vs cluster----
if(cc){ nodes <- 48}
if(!cc | test){ nodes <- 4}

#4. Create and register clusters----
print("* Creating clusters *")
cl <- makePSOCKcluster(nodes, type="PSOCK")

#5. Set root path----
print("* Setting root file path *")
if(cc){root <- "/scratch/ecknight/ABModels"}
if(!cc){root <- "G:/Shared drives/ABMI_ECKnight/Projects/OSM/Deviation From Expected"}

tmpcl <- clusterExport(cl, c("root"))

#6. Load packages on clusters----
print("* Loading packages on workers *")
tmpcl <- clusterEvalQ(cl, library(tidyverse))
tmpcl <- clusterEvalQ(cl, library(AICcmodavg))
tmpcl <- clusterEvalQ(cl, library(MuMIn))

#7. Load data package----
print("* Loading data on master *")

load(file.path(root, "Data", "Train.Rdata"))

#8. Load model script----
source("00.ClimateModels.R")

#9. Number of climate models----
n <- length(modelsclimate)

#10. Link functions----
inv.link  <- function (eta) {pmin(pmax(exp(eta), .Machine$double.eps), .Machine$double.xmax)}
link <- poisson()$linkfun

#11. Load data objects----
print("* Loading data on workers *")

tmpcl <- clusterExport(cl, c("bird_train", "off_train", "covs_train", "boot", "modelsclimate", "n", "inv.link", "link"))

#WRITE FUNCTION##########

model_climate <- function(i){
  
  #1. Loop settings----
  boot.i <- loop$bootstrap[i]
  species.i <- as.character(loop$species[i])
  
  #2. Get the data----
  covs.i <- covs_train[covs_train$surveyid %in% boot[,boot.i],]
  bird.i <- bird_train[bird_train$surveyid %in% boot[,boot.i], species.i]
  off.i <- off_train[off_train$surveyid %in% boot[,boot.i], species.i]
  
  dat.i <- data.frame(count = bird.i, offset = off.i) |>
    cbind(covs.i)
  
  #3. Make model list---
  climate.list <- list()
  
  #4. Run null model----
  climate.list[[1]] <- try(glm(count ~ 1  + offset(offset),
                               data = dat.i,
                               family = "poisson",
                               y=FALSE,
                               model=FALSE))
  
  if(!inherits(climate.list[[1]], "try-error")){
    
    #5. Run the other climate models----
    for(j in 1:n){climate.list[[j + 1]] <- try(update(climate.list[[1]], formula=modelsclimate[[j]]))}
    
    #6. Add XY----
    for(j in 1:n){climate.list[[j + n + 1]] <- try(update(climate.list[[j + 1]], . ~ . + Easting + Northing + Easting:Northing))}
    
    for(j in 1:n){climate.list[[j + 2*n + 1]] <- try(update(climate.list[[j + n + 1]], . ~ . + I(Easting^2) + I(Northing^2)))}
    
    #Take out any try-errors
    climate.list <- climate.list[sapply(climate.list, function(x) !inherits(x, "try-error"))]
    
    #7. Model averaging----
    averagemodel <- model.avg(climate.list , rank = "AICc")
    
    #8. Get predictions----
    averageprediction <- inv.link(predict(averagemodel, type="link", full=TRUE))
    
    #9. Get coefficients----
    averagecoefficients <- data.frame(coef = averagemodel$coefficients["full",])
    
    #10. Save some things----
    
    #Make the model object smaller
    averagemodel$call <- NULL
    averagemodel$coefArray <- NULL
    averagemodel$residuals <- NULL
    
    #Make a species folder in models
    if(!(file.exists(file.path(root, "Results", "ClimateModels", "Models", species.i)))){
      dir.create(file.path(root, "Results", "ClimateModels", "Models", species.i))
    }

    #Save the light model
    save(averagemodel, file = file.path(root, "Results", "ClimateModels", "Models", species.i, paste0("ClimateModel_", species.i, "_", boot.i, ".Rdata")))
    
    #Make a species folder in predictions
    if(!(file.exists(file.path(root, "Results", "ClimateModels", "Predictions", species.i)))){
      dir.create(file.path(root, "Results", "ClimateModels", "Predictions", species.i))
    }
    
    #Save the prediction
    write.csv(averageprediction, file = file.path(root, "Results", "ClimateModels", "Predictions", species.i, paste0("ClimateModelPrediction_", species.i, "_", boot.i, ".csv")), row.names = FALSE)
    
    #Make a species folder in coefficients
    if(!(file.exists(file.path(root, "Results", "ClimateModels", "Coefficients", species.i)))){
      dir.create(file.path(root, "Results", "ClimateModels", "Coefficients", species.i))
    }
    
    #Save the coefficients
    write.csv(averagecoefficients, file = file.path(root, "Results", "ClimateModels", "Coefficients", species.i, paste0("ClimateModelCoefficients_", species.i, "_", boot.i, ".csv")), row.names = TRUE)
  }
  
  #11. Clean up----
  rm(climate.list, averagemodel, averageprediction, averagecoefficients, dat.i)
  
}

#RUN MODELS###############

#1. Set species list----
spp <- c("OVEN")

#2. Set bootstrap list----
b <- c(1:10)

#3. Make todo list----
todo <- expand.grid(species = spp, bootstrap = b) |>
  arrange(species)

#4. Check against models already run----
done <- data.frame(file = list.files(file.path(root, "Results", "ClimateModels", "Predictions"), pattern="*.csv", recursive = TRUE)) |>
  separate(file, into=c("f1", "f2", "species", "bootstrap", "f3")) |> 
  dplyr::select(species, bootstrap) |>
  mutate(bootstrap = as.numeric(bootstrap)) |>
  inner_join(data.frame(file = list.files(file.path(root, "Results", "ClimateModels", "Coefficients"), pattern="*.csv", recursive=TRUE)) |>
               separate(file, into=c("f4", "f5", "species", "bootstrap", "f6")) |>
               dplyr::select(species, bootstrap) |>
               mutate(bootstrap = as.numeric(bootstrap)))

#6. Make modelling list----
loop <- anti_join(todo, done)

if(nrow(loop) > 0){
  
  #For testing
  if(test) {loop <- loop[1:nodes,]}
  
  print("* Loading model loop on workers *")
  tmpcl <- clusterExport(cl, c("loop"))
  
  #7. Run BRT function in parallel----
  print("* Fitting models *")
  mods <- parLapply(cl,
                    X=1:nrow(loop),
                    fun=model_climate)
  
}

#CONCLUDE####

#1. Close clusters----
print("* Shutting down clusters *")
stopCluster(cl)

if(cc){ q() }
