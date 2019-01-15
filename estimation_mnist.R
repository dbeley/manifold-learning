library(vegan)
library(rgl)
library(plot3D)
mnist    <- as.matrix(read.table("data.txt"))
labels <- read.table("labels.txt", colClasses = 'integer')
mnist <- mnist[1:1000, 1:200]

## ESTIMATIONS LLE sur Mnist ----------------------------------------------------------
library(lle)
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
  cpus = 5,
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
source("indicateurs.R")

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
library("FactoMineR")
library("factoextra")
library("corrplot")

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
source("indicateurs.R")
# Isomap
# continuity(k_optimal, mnist, mnist_isomap_fit$points)
# trustworthiness(k_optimal, mnist, mnist_isomap_fit$points)
