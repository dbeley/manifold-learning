## ACP
#library("FactoMineR")
#library("factoextra")
## Isomap
library("vegan")
# LLE
library("lle")

rm(list=ls())

setwd("/home/david/Nextcloud/6. Cours/Manifold Learning/Projet/manifold-learning/")

source("simulation.R")
source("indicateurs.R")

# Données
swissroll <- simuData_swissRoll(1000)
helix <- simuData_helix(1000)
sphere <- simuData_sphere(1000, r=2)
brokenswissroll <- simuData_brokenswissroll(1000, a=0.4, b=0.8)
twinpeaks <- simuData_twinpeaks(1000)

#function esti_lle
#input:
#     data: matrice de donnees
#     k: nombre de voisins à prendre en compte
esti_lle <- function(data,k) {
  bestk <- lle::calc_k(data, m=1)
  res.lle <- lle(sphere, m=1, k=bestk)
  res.lle
}

#function esti_isomap
esti_isomap <- function(data, k) {
  
}

#function esti_acp
esti_acp <- function(data, k) {
  
}

# swissroll, helix, sphere, brokenswissroll, twinpeaks

lle_swissroll <- esti_lle(swissroll)
lle_helix <- esti_lle(helix)
lle_sphere <- esti_lle(sphere)
lle_brokenswissroll <- esti_lle(brokenswissroll)
lle_twinpeaks <- esti_lle(twinpeaks)

distances<- dist(swis,p=2)
x_iso <- isomap(distances, ndim=2, k=5)
plot(x_iso$points, col = jet.col(1000), xlab="", ylab="", cex.axis=1.5)
