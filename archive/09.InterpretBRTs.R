# ---
# title: "09.InterpretBRTs"
# author: "Elly Knight"
# created: "2025-03-03"
# inputs: BRTs of model deviation
# outputs: predictor importance, marginal predictions, and interaction strength from models
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

#WRITE FUNCTION##########

model_interpretation <- function(i){
  
  #1. Loop settings----
  boot.i <- loop$bootstrap[i]
  species.i <- as.character(loop$species[i])
  
  #2. Load the predictions----
  load(file.path(root, "Results", "BRTs", species.i, paste0("BRT_", species.i, "_", boot.i, ".Rdata")))
  
  #3. Get variable importance----
  var.i <- data.frame(m.i$contributions)
  
  #4. Get variable interactions----
  int.i <- data.frame(gbm.interactions(m.i)$rank.list)
  
  #5. Take dataset means---
  mean.i <- dat.i |> 
    summarize_all(mean, na.rm=TRUE)
  
  #5. Get marginal predictions----
  pred <- list()
  for(j in 1:nrow(var.i)){
    
    #get the variable of interest
    var.j <- var.i$var[j]
    
    #take it out of the dataset and make a new 1000 row df
    newdat.j <- data.frame(var = seq(min(dat.i[,var.j]),
                                     max(dat.i[,var.j]),
                                     length.out=1000))
    colnames(newdat.j) <- var.j
    
    #put together with the means for the other vars
    dat.j <- mean.i[,names(mean.i)!=var.j] |> 
      slice(rep(1, 1000)) |> 
      cbind(newdat.j)
    
    #make predictions----
    pred[[j]] <- data.frame(pred = dismo::predict(m.i, dat.j)) |> 
      cbind(dat.j) |> 
      mutate(var = var.j)
    
  }
  
  #put together
  pred.i <- do.call(rbind, pred)
  
  
  #6. Save output ----
  #Make a species folder in predictions
  if(!(file.exists(file.path(root, "Results", "Interpretation", species.i)))){
    dir.create(file.path(root, "Results", "Interpretation", species.i))
  }
  
  save(var.i, int.i, pred.i, file=file.path(root, "Results", "Interpretation", species.i, paste0("Interpretation_", species.i, "_", boot.i, ".Rdata")))
  
}


#RUN MODELS###############

#1. Get list of predictions----
todo <- data.frame(file = list.files(file.path(root, "Results", "BRTs"), pattern="*.Rdata", recursive=TRUE)) |>
  separate(file, into=c("prediction", "spf", "species", "bootstrap", "filetype")) |>
  dplyr::select(-filetype, -spf, -prediction)

#2. Get list of completed BRTs----
done <- data.frame(file = list.files(file.path(root, "Results", "Interpretation"), pattern="*.Rdata", recursive=TRUE)) |>
  separate(file, into=c("interpretation", "spf", "species", "bootstrap", "filetype")) |>
  dplyr::select(-filetype, -spf, -interpretation)

#4. Make todo list----
loop <- anti_join(todo, done) |> 
  dplyr::filter()

if(nrow(loop) > 0){
  
  #For testing
  if(test) {loop <- loop[1:nodes,]}
  
  print("* Loading model loop on workers *")
  tmpcl <- clusterExport(cl, c("loop"))
  
  #4. Run prediction function in parallel----
  print("* Interpreting BRTs *")
  mods <- parLapply(cl,
                    X=1:nrow(loop),
                    fun=model_interpretation)
  
}

#CONCLUDE####

#1. Close clusters----
print("* Shutting down clusters *")
stopCluster(cl)

if(cc){ q() }
