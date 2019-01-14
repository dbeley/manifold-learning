# Ancien fichier, plus utilisé

library("tictoc")
## ACP
#library("FactoMineR")
#library("factoextra")
library(kernlab)
## Isomap
library("vegan")
# LLE
library("lle")

rm(list=ls())
tic("Script entier")

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

#function esti_isomap
esti_isomap <- function(data, k) {
  distances <- dist(data,p=2)
  x_iso <- isomap(distances, ndim=2, k=k)
  x_iso
}

#function esti_acp
esti_acp <- function(data, k) {
  
}

#function esti_tsne
esti_tsne <- function(data, k) {
  
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

trustworthiness(swissroll, lle_swissroll)

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

plot(prcomp(x))

xkpca <- kpca(swissroll, kernel = "rbfdot", kpar = list(sigma = .0001))
plot(pcv(xkpca), col = rep(2:1, each = n), pch = 19)

xkpca <- kpca(swissroll, kernel = "vanilladot", kpar = list())
plot(pcv(xkpca), col = rep(2:1, each = n), pch = 19)

toc()