load("rep_TS/TSboot10.pca_flat.RData")
load("objects.RData")
load("measurements.RData")
source("functions.R")
TSbootmodels10.pca <- vector(mode="list",length=length(TSboot10.pca))
for (i in 1:length(TSboot10.pca)){
  TSbootmodels10.pca[[i]] <- combineRepsTS(TSboot10.pca[[i]])
}
save(TSbootmodels10.pca,file = "rep_TS/TSbootmodels10.pca.RData")
rm(TSboot10.pca,TSbootmodels10.pca)
