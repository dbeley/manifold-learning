library("tictoc")
## Isomap
library("vegan")

rm(list=ls())
tic("Isomap")

source("simulation.R")
source("indicateurs.R")

#function esti_isomap
#input:
#     data: jeu de donnees
#     k: nombre de voisins à prendre en compte
#     ndim: la dimension intrinsèque
esti_isomap <- function(data, k, ndim) {
  distances <- dist(data,p=2)
  x_iso <- isomap(distances, ndim=ndim, k=k)
  x_iso
}

list_data <- list(swissroll, helix, sphere, brokenswissroll, twinpeaks)

##swissroll
#ks <- seq(4, 10, length=9)
##broken
#ks <- seq(11, 50, length=9)
##helix
#ks <- seq(6, 20, length=9)
##twinpeaks
#ks <- seq(4, 20, length=9)
##sphere
#ks <- seq(4, 10, length=9)
#
#par(mfrow=c(3,3))
#for (k in ks) {
#  isomap <- esti_isomap(sphere, k, 2)
#  plot(isomap$points, main=k,col = rainbow(nrow(isomap$points)), pch = 19)
#}
#par(mfrow=c(1,1))

isomap_swissroll <- esti_isomap(swissroll,5,2)
comment(isomap_swissroll) <- "Swissroll"
isomap_helix <- esti_isomap(helix, 10,1)
comment(isomap_helix) <- "Helix"
isomap_sphere <- esti_isomap(sphere, 10,2)
comment(isomap_sphere) <- "Sphere"
isomap_brokenswissroll <- esti_isomap(brokenswissroll, 15,2)
comment(isomap_brokenswissroll) <- "Brokenswissroll"
isomap_twinpeaks <- esti_isomap(twinpeaks,5,2)
comment(isomap_twinpeaks) <- "Twinpeaks"

list_isomap <- list(isomap_swissroll, isomap_helix, isomap_sphere, isomap_brokenswissroll, isomap_twinpeaks)

# plot isomap
par(mfrow=c(1, 2))
for (df in list_isomap) {
  dfnm <- comment(df)
  plot(df$points, main=dfnm,col = rainbow(nrow(df$points)), pch = 19)
}
par(mfrow=c(1, 1))

# trustworthiness
trustworthiness_isomap_swissroll <- trustworthiness(12, swissroll, isomap_swissroll$points)
trustworthiness_isomap_brokenswissroll <- trustworthiness(12, brokenswissroll, isomap_brokenswissroll$points)
trustworthiness_isomap_helix <- trustworthiness(12, helix, isomap_helix$points)
trustworthiness_isomap_twinpeaks <- trustworthiness(12, twinpeaks, isomap_twinpeaks$points)
trustworthiness_isomap_sphere <- trustworthiness(12, sphere, isomap_sphere$points)


# continuity
continuity_isomap_swissroll <- continuity(12, swissroll, isomap_swissroll$points)
continuity_isomap_brokenswissroll <- continuity(12, brokenswissroll, isomap_brokenswissroll$points)
continuity_isomap_helix <- continuity(12, helix, isomap_helix$points)
continuity_isomap_twinpeaks <- continuity(12, twinpeaks, isomap_twinpeaks$points)
continuity_isomap_sphere <- continuity(12, sphere, isomap_sphere$points)

toc()