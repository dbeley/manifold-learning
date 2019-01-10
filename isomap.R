library("tictoc")
## Isomap
library("vegan")

rm(list=ls())
tic("Isomap")

setwd("/home/david/Nextcloud/6. Cours/Manifold Learning/Projet/manifold-learning/")

source("simulation.R")
source("indicateurs.R")

#function esti_isomap
esti_isomap <- function(data, k) {
  distances <- dist(data,p=2)
  x_iso <- isomap(distances, ndim=2, k=k)
  x_iso
}

list_data <- list(swissroll, helix, sphere, brokenswissroll, twinpeaks)

isomap_swissroll <- esti_isomap(swissroll, 5)
#isomap_helix <- esti_isomap(helix, 5)
isomap_sphere <- esti_isomap(sphere, 5)
#isomap_brokenswissroll <- esti_isomap(brokenswissroll, 5)
isomap_twinpeaks <- esti_isomap(twinpeaks, 5)

#list_isomap <- list(isomap_swissroll, isomap_helix, isomap_sphere, isomap_brokenswissroll, isomap_twinpeaks)
list_isomap <- list(isomap_swissroll, isomap_sphere, isomap_twinpeaks)

# plot isomap
par(mfrow=c(1, 2))
lapply(list_isomap, function(x) {plot(x$points, col = jet.col(1000))})
par(mfrow=c(1, 1))

# trustworthiness

# continuity

toc()