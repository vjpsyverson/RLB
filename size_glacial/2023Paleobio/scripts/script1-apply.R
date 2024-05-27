load("rep_TS/TSboot5.RData")
load("objects.RData")
load("measurements.RData")
source("functions.R")
TSbootmodels5 <- vector(mode="list",length=length(meas))
Z <- 1;M <- 1;N <- 1;P <- 1;Q <- 1
m <- n <- z <- p <- q <- 1
#for (m in M:length(TSboot5)) {
#  for (n in N:length(TSboot5[[m]])) {
    for (z in Z:length(TSboot5[[m]][[n]])) {
      TSbootmodels5[[m]][[n]][[z]] <- lapply(TSboot5[[m]][[n]][[z]],function(x) try(modelRepTS(x)))
    }
#  } 
#}
save(TSbootmodels5,file = "rep_TS/TSbootmodels5.RData")
rm(TSboot5,TSbootmodels5)
