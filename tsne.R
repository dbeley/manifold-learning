# Méthode t-SNE

library("tictoc")
# t-SNE
library("Rtsne")

rm(list=ls())
tic("t-SNE")

setwd("/home/david/Nextcloud/6. Cours/Manifold Learning/Projet/manifold-learning/")

source("simulation.R")
source("indicateurs.R")

#function esti_tsne
#input:
#     data: matrice de donnees
#     k: perplexité
esti_tsne <- function(data,k) {
  res.tsne <- Rtsne(data, pca = FALSE, theta = 0.0, perplexity = k)
  res.tsne
}

list_data <- list(swissroll, brokenswissroll, helix, twinpeaks, sphere)

# t-SNE sur swissroll, helix, sphere, brokenswissroll, twinpeaks
tsne_swissroll <- esti_tsne(swissroll, 30)
comment(tsne_swissroll) <- "Swissroll"
tsne_brokenswissroll <- esti_tsne(brokenswissroll, 30)
comment(tsne_brokenswissroll) <- "Brokenswissroll"
tsne_helix <- esti_tsne(helix, 30)
comment(tsne_helix) <- "Helix"
tsne_twinpeaks <- esti_tsne(twinpeaks, 30)
comment(tsne_twinpeaks) <- "Twinpeaks"
tsne_sphere <- esti_tsne(sphere, 30)
comment(tsne_sphere) <- "Sphere"

list_tsne <- list(tsne_swissroll, tsne_brokenswissroll, tsne_helix, tsne_twinpeaks, tsne_sphere)

for (df in list_tsne) {
  dfnm <- comment(df)
  plot(df$Y, main=dfnm)
}

# plot tsne
#par(mfrow=c(1, 2))
#lapply(list_tsne, function(x) {plot(x$Y)})
#par(mfrow=c(1, 1))

# trustworthiness
trustworthiness_tsne_swissroll <- trustworthiness(12, swissroll, tsne_swissroll$Y)
trustworthiness_tsne_brokenswissroll <- trustworthiness(12, brokenswissroll, tsne_brokenswissroll$Y)
trustworthiness_tsne_helix <- trustworthiness(12, helix, tsne_helix$Y)
trustworthiness_tsne_twinpeaks <- trustworthiness(12, twinpeaks, tsne_twinpeaks$Y)
trustworthiness_tsne_sphere <- trustworthiness(12, sphere, tsne_sphere$Y)

# continuity
continuity_tsne_swissroll <- continuity(12, swissroll, tsne_swissroll$Y)
continuity_tsne_brokenswissroll <- continuity(12, brokenswissroll, tsne_brokenswissroll$Y)
continuity_tsne_helix <- continuity(12, helix, tsne_helix$Y)
continuity_tsne_twinpeaks <- continuity(12, twinpeaks, tsne_twinpeaks$Y)
continuity_tsne_sphere <- continuity(12, sphere, tsne_sphere$Y)

# 1-NN
nn_tsne_swissroll <- nn(swissroll, tsne_swissroll$ind$coord)
nn_tsne_brokenswissroll <- nn(brokenswissroll, tsne_brokenswissroll$ind$coord)
nn_tsne_helix <- nn(helix, tsne_helix$ind$coord)
nn_tsne_twinpeaks <- nn(twinpeaks, tsne_twinpeaks$ind$coord)
nn_tsne_sphere <- nn(sphere, tsne_sphere$ind$coord)

toc()
# 87 secondes