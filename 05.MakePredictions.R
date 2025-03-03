# ---
# title: "05.MakePredictions"
# author: "Elly Knight"
# created: "2025-03-03"
# inputs: test data object from `02.WrangleData.R` and model predictions from `03.ModelClimate.R` and `04ModelLandcover.R`
# outputs: out of sample predictions to explore variation in residuals
# notes: 
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

#7. Load data package----
print("* Loading data on master *")

load(file.path(root, "Data", "Test.Rdata"))

#8. Link functions----
inv.link  <- function (eta) {pmin(pmax(exp(eta), .Machine$double.eps), .Machine$double.xmax)}
link <- poisson()$linkfun

#9. Load data objects----
print("* Loading data on workers *")

tmpcl <- clusterExport(cl, c("bird_test", "off_test", "covs_test", "inv.link", "link"))

#WRITE FUNCTION##########

model_predict <- function(i){
  
  #1. Loop settings----
  boot.i <- loop$bootstrap[i]
  species.i <- as.character(loop$species[i])
  
  #2. Load the models----
  load(file.path(root, "Results", "ClimateModels", "Models", species.i, paste0("ClimateModel_", species.i, "_", boot.i, ".Rdata")))
  load(file.path(root, "Results", "LandcoverModels", "Models", species.i, paste0("NorthModel_", species.i, "_", boot.i, ".Rdata")))
  
  #3. Get the data----
  off.i <- off_test[, species.i]
  dat.i <- cbind(covs_test, off.i)
  
  #4. Make the climate predictions----
  clim.i <- inv.link(predict(averagemodel, type="link", full=TRUE, newdata = covs_test))

  

  
}

#RUN MODELS###############

#1. Get list of landcover models----
todo <- data.frame(file = list.files(file.path(root, "Results", "LandCoverModels", "Models"), pattern="*.Rdata", recursive = TRUE)) |>
  separate(file, into=c("f1", "f2", "species", "bootstrap", "f3")) |> 
  dplyr::select(species, bootstrap) |>
  inner_join(data.frame(file = list.files(file.path(root, "Results", "LandCoverModels", "Coefficients"), pattern="*.csv", recursive = TRUE)) |>
               separate(file, into=c("f4", "f5", "species", "bootstrap", "f6")) |>
               dplyr::select(species, bootstrap))

#2. Get list of completed predictions ----
done <- data.frame(file = list.files(file.path(root, "Results", "Predictions"), pattern="*.csv", recursive=TRUE)) |>
  separate(file, into=c("spf", "species", "bootstrap", "filetype")) |>
  dplyr::select(-filetype, -spf)

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
                    fun=model_predict)
  
}




















