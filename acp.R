library("tictoc")
## ACP
#library("FactoMineR")
#library("factoextra")
library(kernlab)

rm(list=ls())
tic("Script entier")

setwd("/home/david/Nextcloud/6. Cours/Manifold Learning/Projet/manifold-learning/")

source("simulation.R")
source("indicateurs.R")

#function esti_acp
esti_acp <- function(data, k) {
  
}

list_data <- list(swissroll, helix, sphere, brokenswissroll, twinpeaks)

plot(prcomp(x))

xkpca <- kpca(swissroll, kernel = "rbfdot", kpar = list(sigma = .0001))
plot(pcv(xkpca), col = rep(2:1, each = n), pch = 19)

xkpca <- kpca(swissroll, kernel = "vanilladot", kpar = list())
plot(pcv(xkpca), col = rep(2:1, each = n), pch = 19)

# trustworthiness

# continuity

toc()