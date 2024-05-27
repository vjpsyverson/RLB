load("rep_TS/TSboot10_flat.RData")
load("objects.RData")
load("measurements.RData")
source("functions.R")
TSbootmodels10 <- vector(mode="list",length=length(TSboot10))
for (i in 1:length(TSboot10)){
  TSbootmodels10[[i]] <- combineRepsTS(TSboot10[[i]])
}
save(TSbootmodels10,file = "rep_TS/TSbootmodels10.RData")
rm(TSboot10,TSbootmodels10)
