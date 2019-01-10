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

# Données
set.seed(20)
swissroll <- simuData_swissRoll(1000)
helix <- simuData_helix(1000)
sphere <- simuData_sphere(1000, r=2)
brokenswissroll <- simuData_brokenswissroll(1000, a=0.4, b=0.8)
twinpeaks <- simuData_twinpeaks(1000)

#function esti_acp
esti_acp <- function(data, k) {
  
}

list_data <- list(swissroll, helix, sphere, brokenswissroll, twinpeaks)

plot(prcomp(x))

xkpca <- kpca(swissroll, kernel = "rbfdot", kpar = list(sigma = .0001))
plot(pcv(xkpca), col = rep(2:1, each = n), pch = 19)

xkpca <- kpca(swissroll, kernel = "vanilladot", kpar = list())
plot(pcv(xkpca), col = rep(2:1, each = n), pch = 19)

toc()