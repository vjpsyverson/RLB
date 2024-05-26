load("rep_TS/TSboot10.pca.RData")
load("objects.RData")
load("measurements.RData")
source("functions.R")
TSbootmodels10.pca <- vector(mode="list",length=length(meas))
Z <- 1;M <- 1;N <- 1;P <- 1;Q <- 1
m <- n <- z <- p <- q <- 1
for (m in M:length(TSboot10.pca)) {
  for (n in N:length(TSboot10.pca[[m]])) {
    for (z in Z:length(TSboot10.pca[[m]][[n]])) {
      TSbootmodels10.pc[[m]][[n]][[z]] <- vector(mode = "list",length = 1000)
      for (p in P:length(TSboot10.pca[[m]][[n]][[z]])) {
        if (all(c(m,n) == c(6,3))) {
          break
        } else {
          TSbootmodels10.pca[[m]][[n]][[z]][[q]] <- 
            try(modelRepTS(TSboot10.pca[[m]][[n]][[z]][[q]]$paleoTS))
        }
      }
    }
  }
}
save(TSbootmodels10.pca,file = "rep_TS/TSbootmodels10.pca.RData")
rm(TSboot10.pca,TSbootmodels10.pca)
