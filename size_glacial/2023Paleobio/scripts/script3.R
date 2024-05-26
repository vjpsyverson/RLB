load("rep_TS/TSboot5.pca.RData")
load("objects.RData")
load("measurements.RData")
source("functions.R")
TSbootmodels5.pca <- vector(mode="list",length=length(meas))
Z <- 1;M <- 1;N <- 1;P <- 1;Q <- 1
m <- n <- z <- p <- q <- 1
for (m in M:length(TSboot5.pca)) {
  for (n in N:length(TSboot5.pca[[m]])) {
    for (z in Z:length(TSboot5.pca[[m]][[n]])) {
      TSbootmodels5.pca[[m]][[n]][[z]] <- vector(mode = "list",length = 1000)
      for (q in Q:length(TSboot5.pca[[m]][[n]][[z]])) {
        if (all(c(m,n) == c(6,3))) {
          break
        } else {
          TSbootmodels5.pca[[m]][[n]][[z]][[q]] <- 
            try(modelRepTS(TSboot5.pca[[m]][[n]][[z]][[q]]$paleoTS))
        }
      }
    }
  }
}
save(TSbootmodels5.pca,file = "rep_TS/TSbootmodels5.pca.RData")
rm(TSboot5.pca,TSbootmodels5.pca)
