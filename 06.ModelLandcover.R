# ---
# title: "04.ModelLandcover"
# author: "Elly Knight"
# created: "2025-03-03"
# inputs: train data object from `02.WrangleData.R` and model predictions from `03.ModelClimate.R`
# outputs: landcover models
# notes: this script is nearly identical to the `07.ModelLandcover.R` scirpt in the BirdModels repo. See that script for details
# ---


#PREAMBLE############################

#1. Load packages----
print("* Loading packages on master *")
library(tidyverse) #basic data wrangling
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
source("00.NorthModels.R")

#9. Load data objects----
print("* Loading data on workers *")

tmpcl <- clusterExport(cl, c("bird_train", "off_train", "covs_train", "boot", "modelsnorth"))

#WRITE FUNCTION##########

model_landcover <- function(i){
  
  #2. Loop settings----
  boot.i <- loop$bootstrap[i]
  species.i <- as.character(loop$species[i])
  
  #3. Get the data----
  covs.i <- covs_train[covs_train$surveyid %in% boot[,boot.i],]
  bird.i <- bird_train[bird_train$surveyid %in% boot[,boot.i], species.i]
  off.i <- off_train[off_train$surveyid %in% boot[,boot.i], species.i]
  
  #4. Load the climate model predictions----
  climate.i <- read.csv(loop$path[i])
  
  #5. Put together----
  dat.i <- data.frame(count = bird.i, offset = off.i, climate = climate.i$x) |>
    cbind(covs.i)

  #8. Make model output list---
  lc.list <- list()
  
  #9. Run null model----
  lc.list[[1]] <- glm(count ~ climate + offset(offset),
                      data = dat.i,
                      family = "poisson",
                      weights = dat.i$vegw)
  
  #10. Set up model stage loop----
  for(j in 1:length(modelsnorth)){
    
    #11. Get models----
    mods.j <- modelsnorth[j][[1]]
    
    #12. Set up model loop----
    full.list <- list()
    for(k in 1:length(mods.j)){
      
      full.list[[k]] <- try(update(lc.list[[j]], formula=mods.j[[k]]))
      
    }
    
    #13. Remove try errors----
    full.list <- full.list[!sapply(full.list, inherits, "try-error")]
    
    if(length(full.list) > 0){
      
      #13. Get the BIC table----
      bictable <- AICcmodavg::bictab(cand.set = full.list, sort = F)
      
      #14. Pick best model----
      bestmodeltable <- bictable |>
        dplyr::filter(K > 1, Delta_BIC <=2) |>
        dplyr::filter(K == min(K))
      
      #15. Save that model for the next stage----
      lc.list[[j+1]] <- full.list[[as.numeric(str_sub(bestmodeltable$Modnames[1], 4, 5))]]
      
    }
    
  }
  
  #16. Get the final model----
  bestmodel <- lc.list[[length(lc.list)]]
  
  #17. Save it----
  #Make a species folder in models
  if(!(file.exists(file.path(root, "Results", "LandcoverModels", "Models", species.i)))){
    dir.create(file.path(root, "Results", "LandcoverModels", "Models", species.i))
  }
  
  #Save the model
  save(bestmodel, file = file.path(root, "Results", "LandcoverModels", "Models", species.i, paste0("NorthModel_", species.i, "_", boot.i, ".Rdata"))) 

  #18. Save the coefficients----
  coef <- data.frame(name = names(bestmodel$coefficients),
                     value = as.numeric(bestmodel$coefficients)) |>
    mutate(name = ifelse(name=="(Intercept)", "Intercept", name))
  
  #Make a species folder in models
  if(!(file.exists(file.path(root, "Results", "LandcoverModels", "Coefficients", species.i)))){
    dir.create(file.path(root, "Results", "LandcoverModels", "Coefficients", species.i))
  }
  
  #Save the coefficients
  write.csv(coef, file = file.path(root, "Results", "LandcoverModels", "Coefficients",species.i, paste0("NorthModel_", species.i, "_", boot.i, ".csv")),  row.names = FALSE)

  #19. Tidy up----
  rm(lc.list, bestmodel, bictable, bestmodeltable, coef)
  
}

#RUN MODELS###############

#1. Get list of climate models----
todo <- data.frame(path = list.files(file.path(root, "Results", "ClimateModels", "Predictions"),  full.names = TRUE, pattern="*.csv", recursive=TRUE),
                      file = list.files(file.path(root, "Results", "ClimateModels", "Predictions"), pattern="*.csv", recursive=TRUE)) |>
  separate(file, into=c("spf", "model", "species", "bootstrap", "filetype")) |>
  dplyr::select(-model, -filetype, -spf)

#2. Check against models already run----
done <- data.frame(file = list.files(file.path(root, "Results", "LandCoverModels", "Models"), pattern="*.Rdata", recursive = TRUE)) |>
  separate(file, into=c("f1", "f2", "species", "bootstrap", "f3")) |> 
  dplyr::select(species, bootstrap) |>
  inner_join(data.frame(file = list.files(file.path(root, "Results", "LandCoverModels", "Coefficients"), pattern="*.csv", recursive = TRUE)) |>
               separate(file, into=c("f4", "f5", "species", "bootstrap", "f6")) |>
               dplyr::select(species, bootstrap))

#3. Make todo list----
loop <- anti_join(todo, done)

if(nrow(loop) > 0){
  
  #For testing
  if(test) {loop <- loop[1:nodes,]}
  
  print("* Loading model loop on workers *")
  tmpcl <- clusterExport(cl, c("loop"))
  
  #4. Run prediction function in parallel----
  print("* Fitting models *")
  mods <- parLapply(cl,
                    X=1:nrow(loop),
                    fun=model_landcover)
  
}

#CONCLUDE####

#1. Close clusters----
print("* Shutting down clusters *")
stopCluster(cl)

if(cc){ q() }
