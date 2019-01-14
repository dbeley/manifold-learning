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
  #bestk <- lle::calc_k(data, m=2)
  #res.lle <- lle(sphere, m=1, k=bestk)
  res.lle <- lle(data, m=2, k=k)
  res.lle
}

list_data <- list(swissroll, brokenswissroll, helix, twinpeaks, sphere)

# meilleur k
# résultat = à peu près 10
#bestk_swissroll <- calc_k(swissroll, m=2)
#bestk_brokenswissroll <- calc_k(brokenswissroll, m=2)
#bestk_helix <- calc_k(helix, m=2)
#bestk_twinpeaks <- calc_k(twinpeaks, m=2)
#bestk_sphere <- calc_k(sphere, m=2)

# LLE sur swissroll, helix, sphere, brokenswissroll, twinpeaks
lle_swissroll <- esti_lle(swissroll, 10)
comment(lle_swissroll) <- "Swissroll"
lle_brokenswissroll <- esti_lle(brokenswissroll, 10)
comment(lle_brokenswissroll) <- "Brokenswissroll"
lle_helix <- esti_lle(helix, 10)
comment(lle_helix) <- "Helix"
lle_twinpeaks <- esti_lle(twinpeaks, 10)
comment(lle_twinpeaks) <- "Twinpeaks"
lle_sphere <- esti_lle(sphere, 10)
comment(lle_sphere) <- "Sphere"

list_lle <- list(lle_swissroll, lle_brokenswissroll, lle_helix, lle_twinpeaks, lle_sphere)

for (df in list_lle) {
  dfnm <- comment(df)
  plot(df$Y, main=dfnm)
}

# plot lle
#par(mfrow=c(1, 2))
#lapply(list_lle, function(x) {plot(x$Y)})
#par(mfrow=c(1, 1))

# trustworthiness
trustworthiness_lle_swissroll <- trustworthiness(12, swissroll, lle_swissroll$Y)
trustworthiness_lle_brokenswissroll <- trustworthiness(12, brokenswissroll, lle_brokenswissroll$Y)
trustworthiness_lle_helix <- trustworthiness(12, helix, lle_helix$Y)
trustworthiness_lle_twinpeaks <- trustworthiness(12, twinpeaks, lle_twinpeaks$Y)
trustworthiness_lle_sphere <- trustworthiness(12, sphere, lle_sphere$Y)

# continuity
continuity_lle_swissroll <- continuity(12, swissroll, lle_swissroll$Y)
continuity_lle_brokenswissroll <- continuity(12, brokenswissroll, lle_brokenswissroll$Y)
continuity_lle_helix <- continuity(12, helix, lle_helix$Y)
continuity_lle_twinpeaks <- continuity(12, twinpeaks, lle_twinpeaks$Y)
continuity_lle_sphere <- continuity(12, sphere, lle_sphere$Y)

toc()
# 132 secondes
