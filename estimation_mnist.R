library("vegan")
library("rgl")
library("plot3D")
library("FactoMineR")
library("factoextra")
library("lle")
library("Rtsne")


rm(list=ls())

#setwd("/home/david/Nextcloud/6. Cours/Manifold Learning/Projet/manifold-learning")
mnist    <- as.matrix(read.table("data.txt"))
labels <-read.table("labels.txt", colClasses = 'integer')
labels <- as.factor(labels$V1)
set.seed(20)
rowsmnist <- sample(5000, 200)
mnist <- mnist[rowsmnist,]
labels <- labels[rowsmnist]

source("indicateurs.R")

# Méthodes
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

#function esti_tsne
#input:
#     data: matrice de donnees
#     k: perplexité
#     ndim: la dimension intrinsèque
esti_tsne <- function(data,k,ndim) {
  res.tsne <- Rtsne(data, dims = ndim, pca = FALSE, theta = 0.0, perplexity = k)
  res.tsne
}

# ACP
acp_mnist <- esti_acp(mnist)
plot(rotated(acp_mnist), main="ACP", col = labels, pch = 19)
legend("topright", legend = levels(labels), col = unique(labels), pch = 19, cex = 0.85)
acp_mnist_res <- rotated(acp_mnist)

trustworthiness_acp_mnist <- trustworthiness(12, mnist, acp_mnist_res)
continuity_acp_mnist <- continuity(12, mnist, acp_mnist_res)

# ACP noyau
acp_noyau_mnist <- esti_acp_noyau(mnist)
acp_noyau_mnist <- esti_acp_noyau(mnist, sigma = 0.0001)
acp_noyau_mnist <- esti_acp_noyau(mnist, sigma = 0.0002)
acp_noyau_mnist <- esti_acp_noyau(mnist, sigma = 0.0002)
plot(pcv(acp_noyau_mnist), main="ACP à noyau (sigma = 0.0002)", col= labels, pch = 19)
legend("topright", legend = levels(labels), col = unique(labels), pch = 19, cex = 0.85)
acp_noyau_mnist_res <- rotated(acp_noyau_mnist)

trustworthiness_acp_noyau_mnist <- trustworthiness(12, mnist, acp_noyau_mnist_res)
continuity_acp_noyau_mnist <- continuity(12, mnist, acp_noyau_mnist_res)

# Isomap
isomap_mnist <- esti_isomap(mnist,5,2)
plot(isomap_mnist$points, main="ISOMAP", col = labels, pch = 19)
legend("topright", legend = levels(labels), col = unique(labels), pch = 19, cex = 0.85)

trustworthiness_isomap_mnist <- trustworthiness(12, mnist, isomap_mnist$points)
continuity_isomap_mnist <- continuity(12, mnist, isomap_mnist$points)

# LLE
k_mnist <- calc_k(
  mnist,
  m = 2,
  kmin = 1,
  kmax = 20,
  #plotres = TRUE,
  plotres = FALSE,
  parallel = TRUE,
  cpus = 4,
  iLLE = FALSE
)
# le k optimal
k_optimal <- as.numeric(which.min(k_mnist$rho))
lle_mnist <- esti_lle(mnist, k_optimal)
plot(lle_mnist$Y, main="LLE", col = labels, pch = 19)
legend("topright", legend = levels(labels), col = unique(labels), pch = 19, cex = 0.85)

trustworthiness_lle_mnist <- trustworthiness(12, mnist, lle_mnist$Y)
continuity_lle_mnist <- continuity(12, mnist, lle_mnist$Y)

# t-SNE
tsne_mnist <- esti_tsne(mnist, 30,2)
plot(tsne_mnist$Y, main="t-SNE", col = labels, pch = 19)
legend("topright", legend = levels(labels), col = unique(labels), pch = 19, cex = 0.85)

trustworthiness_tsne_mnist <- trustworthiness(12, mnist, tsne_mnist$Y)
continuity_tsne_mnist <- continuity(12, mnist, tsne_mnist$Y)
