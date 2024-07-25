load("rep_TS/TSboot5_flat.RData")
load("objects.RData")
load("measurements.RData")
source("functions.R")
TSbootmodels5 <- TScombinedresults5 <- vector(mode = "list",length = length(TSboot5))
for (i in 1:length(TSboot5)){
  TSbootmodels5[[i]] <- modelRepsTS(TSboot5[[i]][1:250])
  TScombinedresults5[[i]] <- combineRepsTS(TSbootmodels5[[i]])
}
names(TSbootmodels5) <- names(TScombinedresults5) <- names(TSboot5)
save(TSbootmodels5, TScombinedresults5, file = "rep_TS/TSbootmodels5.RData")
rm(TSboot5,TSbootmodels5, TScombinedresults5)
