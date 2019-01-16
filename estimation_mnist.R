library("vegan")
library("rgl")
library("plot3D")
library("FactoMineR")
library("factoextra")
library("corrplot")
library("lle")
library("Rtsne")
setwd("/home/david/Nextcloud/6. Cours/Manifold Learning/Projet/manifold-learning")
mnist    <- as.matrix(read.table("data.txt"))
labels <- read.table("labels.txt", colClasses = 'integer')
mnist <- mnist[1:500,]

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


## ESTIMATIONS LLE sur Mnist ----------------------------------------------------------
set.seed(24)
# Estimation LLE de Swiss Roll---------------------
# k optimal
k_mnist <- calc_k(
  mnist,
  m = 2,
  kmin = 1,
  kmax = 20,
  plotres = TRUE,
  parallel = TRUE,
  cpus = 4,
  iLLE = FALSE
)
# le k optimal
k_optimal <- as.numeric(which.min(k_mnist$rho))
mnist_lle_fit <- lle(mnist,
                  m = 2,
                  k = k_optimal,
                  id = TRUE)
str(mnist_lle_fit)

# plot results and intrinsic dimension (manually)

plot(
  mnist_lle_fit$Y,
  main = "embedded data",
  xlab = expression(y[1]),
  ylab = expression(y[2]),
  col = rainbow(nrow(mnist)),
  pch = 19
)

plot(
  mnist_lle_fit$id,
  main = "intrinsic dimension",
  xlab = expression(x[i]),
  ylab = "id",
  col = rainbow(nrow(mnist)),
  pch = 19
)


# Plot en  3D
plot3d(
  mnist_lle_fit$Y,
  main = "embedded data",
  xlab = expression(y[1]),
  ylab = expression(y[2]),
  col = rainbow(nrow(mnist)),
  pch = 19
)

# ESTIMATION ISOMAP sur mnist-----------------------------------------------------------

mnist_isomap_fit <- isomap(dist(mnist), ndim = 2, k = k_optimal)
plot(mnist_isomap_fit$eig[1:20] / mnist_isomap_fit$eig[1],
     type = 'h',
     main = "Normalized eigenvalues from Isomap")
plot(mnist_isomap_fit$points, col = rainbow(nrow(mnist)), pch = 19)

# Estimation MNIST t-SNE -------------------------------------

mnist_tsne_fit <- Rtsne(dist(mnist), 
                        pca = FALSE,
                        theta = 0.0,
                        perplexity = 30 ) 
plot(mnist_tsne_fit$Y,  col = rainbow(nrow(mnist)), pch=19)

# Estimation ACP ---------------------------------------------

mnist_acp_fit <- PCA(mnist, scale.unit= TRUE, graph = T)
print(mnist_acp_fit)
eig.val <- get_eigenvalue(mnist_acp_fit)
eig.val

fviz_eig(mnist_acp_fit, addlabels = TRUE, ylim = c(0, 50))

var <- get_pca_var(mnist_acp_fit)
var
# Cercle de correlation
fviz_pca_var(mnist_acp_fit, col.var = "red")

# Cos2 total des variables sur Dim.1 et Dim.2
fviz_cos2(mnist_acp_fit, choice = "var", axes = 1:2, xlim = c(0, 30))

# Mesures d'évaluation ------------------------------------

# ACP
acp_mnist <- esti_acp(mnist)
plot(rotated(df), main=dfnm)
acp_mnist_res <- rotated(acp_mnist)

trustworthiness_acp_mnist <- trustworthiness(12, mnist, acp_mnist_res)
continuity_acp_mnist <- continuity(12, mnist, acp_mnist_res)

# ACP noyau
acp_noyau_mnist <- esti_acp_noyau(mnist)
plot(pcv(df), main=dfnm)
acp_noyau_mnist_res <- rotated(acp_noyau_mnist)

trustworthiness_acp_noyau_mnist <- trustworthiness(12, mnist, acp_noyau_mnist_res)
continuity_acp_noyau_mnist <- continuity(12, mnist, acp_noyau_mnist_res)

# Isomap
isomap_mnist <- esti_isomap(mnist,5,2)
plot(df$points, main=dfnm,col = rainbow(nrow(df$points)))

trustworthiness_isomap_mnist <- trustworthiness(12, mnist, isomap_mnist$points)
continuity_isomap_mnist <- continuity(12, mnist, isomap_mnist$points)

# LLE
lle_mnist <- esti_lle(mnist, 10)
plot(df$Y, main=dfnm)

trustworthiness_lle_mnist <- trustworthiness(12, mnist, lle_mnist$Y)
continuity_lle_mnist <- continuity(12, mnist, lle_mnist$Y)

# t-SNE
tsne_mnist <- esti_tsne(mnist, 30,2)
plot(df$Y, main=dfnm)

trustworthiness_tsne_mnist <- trustworthiness(12, mnist, tsne_mnist$Y)
continuity_tsne_mnist <- continuity(12, mnist, tsne_mnist$Y)

# continuity(k_optimal, mnist, mnist_isomap_fit$points)
# trustworthiness(k_optimal, mnist, mnist_isomap_fit$points)
