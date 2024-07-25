load("rep_TS/TSboot5.pca_flat.RData")
load("objects.RData")
load("measurements.RData")
source("functions.R")
TSbootmodels5.pca <- TScombinedresults5.pca <- vector(mode="list",length=length(TSboot5.pca))
for (i in 1:length(TSboot5.pca)){
  TSbootmodels5.pca[[i]] <- modelRepsTS(TSboot5.pca[[i]][1:250])
  TScombinedresults5.pca[[i]] <- combineRepsTS(TSbootmodels5.pca[[i]])
}
names(TSbootmodels5.pca) <- names(TScombinedresults5.pca) <- names(TSboot5.pca)
save(TSbootmodels5.pca, TScombinedresults5.pca, file = "rep_TS/TSbootmodels5.pca.RData")
rm(TSboot5.pca,TSbootmodels5.pca, TScombinedresults5.pca)
