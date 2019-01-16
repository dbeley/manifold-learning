# Méthode ACP simple et à noyau

library("tictoc")
## ACP
library("FactoMineR")
library("factoextra")
library("kernlab")

rm(list=ls())
tic("ACP")

#setwd("/home/david/Nextcloud/6. Cours/Manifold Learning/Projet/manifold-learning/")

source("simulation.R")
source("indicateurs.R")

#function esti_acp
#input:
#     data: matrice de donnees
esti_acp <- function(data) {
  res.acp <- kpca(data, kernel="vanilladot", features = 2, kpar=list())
  #res.acp <- PCA(data, graph=FALSE)
  res.acp
}

esti_acp_noyau <- function(data, kernel="rbfdot", sigma = 0.0001) {
  res.acp <- kpca(data, kernel=kernel, features=2, kpar=list(sigma = sigma))
  res.acp
}

list_data <- list(swissroll, brokenswissroll, helix, twinpeaks, sphere)

# ACP simple sur swissroll, helix, sphere, brokenswissroll, twinpeaks
acp_swissroll <- esti_acp(swissroll)
comment(acp_swissroll) <- "Swissroll"
acp_brokenswissroll <- esti_acp(brokenswissroll)
comment(acp_brokenswissroll) <- "Brokenswissroll"
acp_helix <- esti_acp(helix)
comment(acp_helix) <- "Helix"
acp_twinpeaks <- esti_acp(twinpeaks)
comment(acp_twinpeaks) <- "Twinpeaks"
acp_sphere <- esti_acp(sphere)
comment(acp_sphere) <- "Sphere"

list_acp <- list(acp_swissroll, acp_brokenswissroll, acp_helix, acp_twinpeaks, acp_sphere)

for (df in list_acp) {
  dfnm <- comment(df)
  #plot(df$ind$coord, main=dfnm)
  plot(rotated(df), main=dfnm, col = rainbow(nrow(df)))
}

# coordonnées 2d de la projection
acp_swissroll_res <- rotated(acp_swissroll)
acp_brokenswissroll_res <- rotated(acp_brokenswissroll) 
acp_helix_res <- rotated(acp_helix)[,1]
acp_twinpeaks_res <- rotated(acp_twinpeaks)
acp_sphere_res <- rotated(acp_sphere)

# trustworthiness
trustworthiness_acp_swissroll <- trustworthiness(12, swissroll, acp_swissroll_res)
trustworthiness_acp_brokenswissroll <- trustworthiness(12, brokenswissroll, acp_brokenswissroll_res)
trustworthiness_acp_helix <- trustworthiness(12, helix, acp_helix_res)
trustworthiness_acp_twinpeaks <- trustworthiness(12, twinpeaks, acp_twinpeaks_res)
trustworthiness_acp_sphere <- trustworthiness(12, sphere, acp_sphere_res)

# continuity
continuity_acp_swissroll <- continuity(12, swissroll, acp_swissroll_res)
continuity_acp_brokenswissroll <- continuity(12, brokenswissroll, acp_brokenswissroll_res)
continuity_acp_helix <- continuity(12, helix, acp_helix_res)
continuity_acp_twinpeaks <- continuity(12, twinpeaks, acp_twinpeaks_res)
continuity_acp_sphere <- continuity(12, sphere, acp_sphere_res)

# Proportion de mêmes voisins
pp_acp_swissroll <- pp(swissroll, acp_swissroll_res)
pp_acp_brokenswissroll <- pp(brokenswissroll, acp_brokenswissroll_res)
pp_acp_helix <- pp(helix, acp_helix_res)
pp_acp_twinpeaks <- pp(twinpeaks, acp_twinpeaks_res)
pp_acp_sphere <- pp(sphere, acp_sphere_res)

# Noyau gaussien

# ACP à noyau
acp_noyau_swissroll <- esti_acp_noyau(swissroll)
comment(acp_noyau_swissroll) <- "Swissroll"
acp_noyau_brokenswissroll <- esti_acp_noyau(brokenswissroll)
comment(acp_noyau_brokenswissroll) <- "Brokenswissroll"
acp_noyau_helix <- esti_acp_noyau(helix)
comment(acp_noyau_helix) <- "Helix"
acp_noyau_twinpeaks <- esti_acp_noyau(twinpeaks)
comment(acp_noyau_twinpeaks) <- "Twinpeaks"
acp_noyau_sphere <- esti_acp_noyau(sphere)
comment(acp_noyau_sphere) <- "Sphere"

list_acp_noyau <- list(acp_noyau_swissroll, acp_noyau_brokenswissroll, acp_noyau_helix, acp_noyau_twinpeaks, acp_noyau_sphere)

for (df in list_acp_noyau) {
  dfnm <- comment(df)
  plot(pcv(df), main=dfnm)
}

# coordonnées 2d de la projection
acp_noyau_swissroll_res <- rotated(acp_noyau_swissroll)
acp_noyau_brokenswissroll_res <- rotated(acp_noyau_brokenswissroll) 
acp_noyau_helix_res <- rotated(acp_noyau_helix)
acp_noyau_twinpeaks_res <- rotated(acp_noyau_twinpeaks)
acp_noyau_sphere_res <- rotated(acp_noyau_sphere)

# trustworthiness
trustworthiness_acp_noyau_swissroll <- trustworthiness(12, swissroll, acp_noyau_swissroll_res)
trustworthiness_acp_noyau_brokenswissroll <- trustworthiness(12, brokenswissroll, acp_noyau_brokenswissroll_res)
trustworthiness_acp_noyau_helix <- trustworthiness(12, helix, acp_noyau_helix_res)
trustworthiness_acp_noyau_twinpeaks <- trustworthiness(12, twinpeaks, acp_noyau_twinpeaks_res)
trustworthiness_acp_noyau_sphere <- trustworthiness(12, sphere, acp_noyau_sphere_res)

# continuity
continuity_acp_noyau_swissroll <- continuity(12, swissroll, acp_noyau_swissroll_res)
continuity_acp_noyau_brokenswissroll <- continuity(12, brokenswissroll, acp_noyau_brokenswissroll_res)
continuity_acp_noyau_helix <- continuity(12, helix, acp_noyau_helix_res)
continuity_acp_noyau_twinpeaks <- continuity(12, twinpeaks, acp_noyau_twinpeaks_res)
continuity_acp_noyau_sphere <- continuity(12, sphere, acp_noyau_sphere_res)

toc()
