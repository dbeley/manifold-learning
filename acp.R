# Méthode ACP simple et à noyau

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
  res.acp <- PCA(data, graph=FALSE)
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

# 1-NN
nn_acp_swissroll <- nn(swissroll, acp_swissroll$ind$coord)
nn_acp_brokenswissroll <- nn(brokenswissroll, acp_brokenswissroll$ind$coord)
nn_acp_helix <- nn(helix, acp_helix$ind$coord)
nn_acp_twinpeaks <- nn(twinpeaks, acp_twinpeaks$ind$coord)
nn_acp_sphere <- nn(sphere, acp_sphere$ind$coord)

# liste des kernel
# rbfdot : Radial Basis kernel function "Gaussian"
# polydot : Polynomial kernel function
# vanilladot : Linear kernel function
# tanhdot : Hyperbolic tangent kernel function
# laplacedot : Laplacian kernel function
# besseldot : Bessel kernel function
# anovadot : ANOVA RBF kernel function
# splinedot : Spline kernel
liste_noyaux <- c("rbfdot", "polydot", "vanilladot", "tanhdot", "laplacedot", "besseldot", "anovadot", "splinedot")

# ne marche pas bien
#liste_noyaux <- c("rbfdot", "laplacedot", "besseldot", "anovadot")
#liste_noyaux <- c("rbfdot", "polydot")
#for (i in liste_noyaux) {
#  if (i %in% c("rbfdot", "laplacedot", "besseldot", "anovadot")) {
#    par(mfrow=c(3, 3))
#    #sigmas = 10^seq(-5, -3, length=9)
#    sigmas = 10^seq(-10, 3, length=9)
#    for (sigma in sigmas) {
#      acp_noyau_swissroll <- esti_acp_noyau(swissroll, i, sigma)
#      #xkpca <- kpca(x, kernel = "rbfdot", kpar = list(sigma = sigma))
#      plot(pcv(acp_noyau_swissroll), col = rep(2:1, each = nrow(acp_noyau_swissroll)), pch = 19, main = paste(i, " : ", sigma))
#  }
#  par(mfrow=c(1, 1))
#
#  }
#  trustworthiness_acp_noyau_swissroll <- trustworthiness(12, swissroll, pcv((acp_noyau_swissroll)))
#  print(paste("Trustworthiness noyau ", i, " : ", trustworthiness_acp_noyau_swissroll))
#  continuity_acp_noyau_swissroll <- continuity(12, swissroll, pcv((acp_noyau_swissroll)))
#  print(paste("Continuity noyau ", i, " : ", continuity_acp_noyau_swissroll))
#}

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

# trustworthiness
trustworthiness_acp_noyau_swissroll <- trustworthiness(12, swissroll, rotated(acp_noyau_swissroll))
trustworthiness_acp_noyau_brokenswissroll <- trustworthiness(12, brokenswissroll, rotated(acp_noyau_brokenswissroll))
trustworthiness_acp_noyau_helix <- trustworthiness(12, helix, rotated(acp_noyau_helix))
trustworthiness_acp_noyau_twinpeaks <- trustworthiness(12, twinpeaks, rotated(acp_noyau_twinpeaks))
trustworthiness_acp_noyau_sphere <- trustworthiness(12, sphere, rotated(acp_noyau_sphere))

# continuity
continuity_acp_noyau_swissroll <- continuity(12, swissroll, rotated(acp_noyau_swissroll))
continuity_acp_noyau_brokenswissroll <- continuity(12, brokenswissroll, rotated(acp_noyau_brokenswissroll))
continuity_acp_noyau_helix <- continuity(12, helix, rotated(acp_noyau_helix))
continuity_acp_noyau_twinpeaks <- continuity(12, twinpeaks, rotated(acp_noyau_twinpeaks))
continuity_acp_noyau_sphere <- continuity(12, sphere, rotated(acp_noyau_sphere))

# 1-NN
nn_acp_noyau_swissroll <- nn(swissroll, rotated(acp_noyau_swissroll))
nn_acp_noyau_brokenswissroll <- nn(brokenswissroll, rotated(acp_noyau_brokenswissroll))
nn_acp_noyau_helix <- nn(helix, rotated(acp_noyau_helix))
nn_acp_noyau_twinpeaks <- nn(twinpeaks, rotated(acp_noyau_twinpeaks))
nn_acp_noyau_sphere <- nn(sphere, rotated(acp_noyau_sphere))

toc()
