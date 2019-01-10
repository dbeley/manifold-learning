library("tictoc")
## ACP
library("FactoMineR")
library("factoextra")
library("kernlab")

rm(list=ls())
tic("ACP")

setwd("/home/david/Nextcloud/6. Cours/Manifold Learning/Projet/manifold-learning/")

source("simulation.R")
source("indicateurs.R")

#function esti_acp
#input:
#     data: matrice de donnees
esti_acp <- function(data) {
  res.acp <- PCA(data)
  res.acp
}

esti_acp_noyau <- function(data) {
  res.acp <- kpca(data, kernel="rbfdot", kpar=list(sigma = .0001))
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
  plot(df$ind$coord, main=dfnm)
}

# trustworthiness
trustworthiness_acp_swissroll <- trustworthiness(12, swissroll, acp_swissroll$ind$coord)
trustworthiness_acp_brokenswissroll <- trustworthiness(12, brokenswissroll, acp_brokenswissroll$ind$coord)
trustworthiness_acp_helix <- trustworthiness(12, helix, acp_helix$ind$coord)
trustworthiness_acp_twinpeaks <- trustworthiness(12, twinpeaks, acp_twinpeaks$ind$coord)
trustworthiness_acp_sphere <- trustworthiness(12, sphere, acp_sphere$ind$coord)

# continuity
continuity_acp_swissroll <- continuity(12, swissroll, acp_swissroll$ind$coord)
continuity_acp_brokenswissroll <- continuity(12, brokenswissroll, acp_brokenswissroll$ind$coord)
continuity_acp_helix <- continuity(12, helix, acp_helix$ind$coord)
continuity_acp_twinpeaks <- continuity(12, twinpeaks, acp_twinpeaks$ind$coord)
continuity_acp_sphere <- continuity(12, sphere, acp_sphere$ind$coord)

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
  plot(pcv(df)[1:2,], main=dfnm)
}

# trustworthiness
trustworthiness_acp_noyau_swissroll <- trustworthiness(12, swissroll, acp_noyau_swissroll$Y)
trustworthiness_acp_noyau_brokenswissroll <- trustworthiness(12, brokenswissroll, acp_noyau_brokenswissroll$Y)
trustworthiness_acp_noyau_helix <- trustworthiness(12, helix, acp_noyau_helix$Y)
trustworthiness_acp_noyau_twinpeaks <- trustworthiness(12, twinpeaks, acp_noyau_twinpeaks$Y)
trustworthiness_acp_noyau_sphere <- trustworthiness(12, sphere, acp_noyau_sphere$Y)

# continuity
continuity_acp_noyau_swissroll <- continuity(12, swissroll, acp_noyau_swissroll$Y)
continuity_acp_noyau_brokenswissroll <- continuity(12, brokenswissroll, acp_noyau_brokenswissroll$Y)
continuity_acp_noyau_helix <- continuity(12, helix, acp_noyau_helix$Y)
continuity_acp_noyau_twinpeaks <- continuity(12, twinpeaks, acp_noyau_twinpeaks$Y)
continuity_acp_noyau_sphere <- continuity(12, sphere, acp_noyau_sphere$Y)

toc()