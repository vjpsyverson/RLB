load("rep_TS/TSboot5_flat.RData")
load("objects.RData")
load("measurements.RData")
source("functions.R")
TSbootmodels5 <- vector(mode="list",length=length(TSboot5))
for (i in 1:length(TSboot5)){
  TSbootmodels5[[i]] <- combineRepsTS(TSboot5[[i]])
}
save(TSbootmodels5,file = "rep_TS/TSbootmodels5.RData")
rm(TSboot5,TSbootmodels5)
