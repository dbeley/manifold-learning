
library("tictoc")
# LLE
library("lle")

rm(list=ls())
tic("LLE")

setwd("/home/david/Nextcloud/6. Cours/Manifold Learning/Projet/manifold-learning/")

source("simulation.R")
source("indicateurs.R")

#function esti_lle
#input:
#     data: matrice de donnees
#     k: nombre de voisins à prendre en compte
esti_lle <- function(data,k) {
  #bestk <- lle::calc_k(data, m=1)
  #res.lle <- lle(sphere, m=1, k=bestk)
  res.lle <- lle(data, m=2, k=k)
  res.lle
}

list_data <- list(swissroll, helix, sphere, brokenswissroll, twinpeaks)

# swissroll, helix, sphere, brokenswissroll, twinpeaks
#lle_swissroll <- esti_lle(swissroll)
lle_helix <- esti_lle(helix, 5)
lle_sphere <- esti_lle(sphere, 5)
lle_brokenswissroll <- esti_lle(brokenswissroll, 5)
lle_twinpeaks <- esti_lle(twinpeaks, 5)

#list_lle <- list(lle_swissroll, lle_helix, lle_sphere, lle_brokenswissroll, lle_twinpeaks)
list_lle <- list(lle_helix, lle_sphere, lle_brokenswissroll, lle_twinpeaks)

# plot lle
par(mfrow=c(1, 2))
lapply(list_lle, function(x) {plot(x$X, x$Y)})
par(mfrow=c(1, 1))

# trustworthiness
trustworthiness(swissroll, lle_swissroll)

# continuity

toc()