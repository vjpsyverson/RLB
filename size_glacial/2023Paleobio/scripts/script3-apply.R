load("rep_TS/TSboot5.pca_flat.RData")
load("objects.RData")
load("measurements.RData")
source("functions.R")
TSbootmodels5 <- vector(mode="list",length=length(TSboot5.pca))
for (i in 1:length(TSboot5.pca)){
  TSbootmodels5.pca[[i]] <- combineRepsTS(TSboot5.pca[[i]])
}
save(TSbootmodels5.pca,file = "rep_TS/TSbootmodels5.pca.RData")
rm(TSboot5.pca,TSbootmodels5.pca)
