load("rep_TS/TSboot10.RData")
load("objects.RData")
load("measurements.RData")
source("functions.R")
TSbootmodels10 <- vector(mode="list",length=length(meas))
Z <- 1;M <- 1;N <- 1;P <- 1;Q <- 1
m <- n <- z <- p <- q <- 1
for (m in M:length(TSboot10)) {
  for (n in N:length(TSboot10[[m]])) {
    for (z in Z:length(TSboot10[[m]][[n]])) {
      TSbootmodels10[[m]][[n]][[z]] <- vector(mode = "list",length = 1000)
      for (p in P:length(TSboot10[[m]][[n]][[z]])) {
        TSbootmodels10[[m]][[n]][[z]][[p]] <- 
          try(modelRepTS(TSboot10[[m]][[n]][[z]][[p]]$paleoTS))
      }
    }
  }
}
save(TSbootmodels10,file = "rep_TS/TSbootmodels10.RData")
rm(TSboot10,TSbootmodels10)
