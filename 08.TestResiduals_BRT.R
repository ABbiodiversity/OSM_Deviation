# ---
# title: "08.TestResiduals"
# author: "Elly Knight"
# created: "2025-03-03"
# inputs: model predictions
# outputs: boosted regression tree model of deviation from predictions & BRT model performance
# notes:

# ---

#1. Load packages----
print("* Loading packages on master *")
library(tidyverse) #basic data wrangling
library(parallel) #parallel computing
library(dismo) #BRTs

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
tmpcl <- clusterEvalQ(cl, library(dismo))

#7. Load data package----
print("* Loading data on master *")

load(file.path(root, "Data", "Test.Rdata"))

#8. Tidy the column names and pick covariates----
covs_use <- covs_badr |> 
  rename(badr_highwells = `High Activity Insitu Well Pads`,
         badr_roads = Roads,
         badr_mine = `Plant/Mine`,
         badr_linear = `Dense Linear Features`,
         badr_lowwells = `Low Activity Well Pads`,
         badr_minebuffer = `Plant/Mine Buffer`,
         badr_reference = `Low Disturbance/Reference`) |> 
  dplyr::select(surveyid, year, cei, propmine, proproad, propseismi, propallwel, proppipe, badr_highwells, badr_roads, badr_mine, badr_linear, badr_lowwells, badr_minebuffer)

#9. Load data objects----
print("* Loading data on workers *")

tmpcl <- clusterExport(cl, c("covs_use"))

#WRITE FUNCTION##########

model_deviation <- function(i){
  
  t0 <- proc.time()
  
  #1. Loop settings----
  boot.i <- loop$bootstrap[i]
  species.i <- as.character(loop$species[i])
  lr.i <- as.numeric(loop$lr[i])
  
  #2. Load the predictions----
  pred.i <- read.csv(file.path(root, "Results", "Predictions", species.i, paste0("Predictions_", species.i, "_", boot.i, ".csv")))
  
  #3. Bind to covariates -----
  # pick the ones we want
  dat.i <- pred.i |>
    dplyr::select(surveyid, residual) |> 
    inner_join(covs_use) |> 
    dplyr::select(-surveyid)
  
  #4. Run model ----
  set.seed(1234)
  m.i <- try(dismo::gbm.step(data=dat.i,
                             gbm.x=c(2:ncol(dat.i)),
                             gbm.y=1,
                             tree.complexity = 2,
                             learning.rate = lr.i,
                             family = "gaussian"))
  if(inherits(m.i, "try-error")){ return(NULL)}

  #5. Save model ----
  #Make a species folder in predictions
  if(!(file.exists(file.path(root, "Results", "BRTs", species.i)))){
    dir.create(file.path(root, "Results", "BRTs", species.i))
  }
  
  #also store the dataset
  save(m.i, dat.i, file=file.path(root, "Results", "BRTs", species.i, paste0("BRT_", species.i, "_", boot.i, ".Rdata")))
  
  #6. Save performance ----
  out.i <- loop[i,] |> 
    cbind(data.frame(trees = m.i$n.trees,
                     deviance.mean = m.i$cv.statistics$deviance.mean,
                     deviance.se = m.i$cv.statistics$deviance.se,
                     null = m.i$self.statistics$mean.null,
                     resid = m.i$self.statistics$mean.resid,
                     correlation = m.i$self.statistics$correlation,
                     correlation.mean = m.i$cv.statistics$correlation.mean,
                     correlation.se = m.i$cv.statistics$correlation.se,
                     time = (proc.time()-t0)[3]))
  
  #Make a species folder in predictions
  if(!(file.exists(file.path(root, "Results", "BRTs", "ModelPerformance")))){
    dir.create(file.path(root, "Results", "BRTs", "ModelPerformance"))
  }
  
  write.csv(out.i, file=file.path(root, "Results", "BRTs", "ModelPerformance", paste0(species.i, "_", boot.i, ".csv")), row.names = FALSE)
  
}

#RUN MODELS###############

#1. Get list of predictions----
todo <- data.frame(file = list.files(file.path(root, "Results", "Predictions"), pattern="*.csv", recursive=TRUE)) |>
  separate(file, into=c("prediction", "spf", "species", "bootstrap", "filetype")) |>
  dplyr::select(-filetype, -spf, -prediction)

#2. Set the learning rates----
lr <- data.frame(species = unique(todo$species)) |> 
  mutate(lr = case_when(species %in% c("LEFL", "PIWO") ~ 0.005,
                        species %in% c("HETH") ~ 0.01,
                        species %in% c("OVEN") ~ 0.05,
                        !is.na(species) ~ 0.001))

#3. Get list of completed BRTs----
done <- data.frame(file = list.files(file.path(root, "Results", "BRTs"), pattern="*.Rdata", recursive=TRUE)) |>
  separate(file, into=c("prediction", "spf", "species", "bootstrap", "filetype")) |>
  dplyr::select(-filetype, -spf, -prediction)

#4. Make todo list----
loop <- anti_join(todo, done) |> 
  left_join(lr)

if(nrow(loop) > 0){
  
  #For testing
  if(test) {loop <- loop[1:nodes,]}
  
  print("* Loading model loop on workers *")
  tmpcl <- clusterExport(cl, c("loop"))
  
  #4. Run prediction function in parallel----
  print("* Running BRTs *")
  mods <- parLapply(cl,
                    X=1:nrow(loop),
                    fun=model_deviation)
  
}


#CONCLUDE####

#1. Close clusters----
print("* Shutting down clusters *")
stopCluster(cl)

if(cc){ q() }
