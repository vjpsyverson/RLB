load("rep_TS/TSboot10.pca_flat.RData")
load("objects.RData")
load("measurements.RData")
source("functions.R")
TSbootmodels10.pca <- TScombinedresults10.pca <- vector(mode="list",length=length(TSboot10.pca))
for (i in 1:length(TSboot10.pca)){
  TSbootmodels10.pca[[i]] <- modelRepsTS(TSboot10.pca[[i]][1:250])
  TScombinedresults10.pca[[i]] <- combineRepsTS(TSbootmodels10.pca[[i]])
}
names(TSbootmodels10.pca) <- names(TScombinedresults10.pca) <- names(TSboot10.pca)
save(TSbootmodels10.pca, TScombinedresults10.pca, file = "rep_TS/TSbootmodels10.pca.RData")
rm(TSboot10.pca,TSbootmodels10.pca, TScombinedresults10.pca)
