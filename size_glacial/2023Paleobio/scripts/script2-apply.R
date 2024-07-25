load("rep_TS/TSboot10_flat.RData")
load("objects.RData")
load("measurements.RData")
source("functions.R")
TSbootmodels10 <- TScombinedresults10 <- vector(mode="list",length=length(TSboot10))
for (i in 1:length(TSboot10)){
  TSbootmodels10[[i]] <- modelRepsTS(TSboot10[[i]][1:250])
  TScombinedresults10[[i]] <- combineRepsTS(TSbootmodels10[[i]])
}
names(TSbootmodels10) <- names(TScombinedresults10) <- names(TSboot10)
save(TSbootmodels10, TScombinedresults10, file = "rep_TS/TSbootmodels10.RData")
rm(TSboot10,TSbootmodels10, TScombinedresults10)
