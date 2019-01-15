rm(list = ls())

## SIMULATION DES JEUX DE DONNEES---------------------------------------------------------------------
library(rgl) # nice 3d graphs
library(vegan)
library(lle)# fit lle

# Dataset Swiss roll ####
simuData_swissroll <- function(n) {
  #n <- 1000 # Random position on the parameteric domain.
  u <- matrix(runif(2 * n), ncol = 2)
  v <- 3 * pi / 2 * (1 + 2 * u[, 1])
  x <- cos(v) * v
  y <- 30 * u[, 2]
  z <- sin(v) * v
  swissroll <- cbind(x, y , z)
  swissroll[order(v), ]
}

swissroll <- simuData_swissroll(1000)
plot3d(swissroll, col = rainbow(nrow(swissroll)), size = 10)
scatter3D()
# Dataset Broken Swiss roll ####
simuData_brokenswissroll <- function(n) {
  a <- 0.4
  b <- 0.8
  u <- matrix(runif(2 * n), nrow = n, ncol = 2)
  u <- u[which(u[, 1] < a | u[, 1] > b),]
  v <- 3 * pi / 2 * (1 + 2 * u[, 1])
  x <- cos(v) * v
  y <- sin(v) * v
  z <- 30 * u[, 2]
  bswissroll <- cbind(x, y , z)
  bswissroll[order(v), ]
}

bswissroll <- simuData_brokenswissroll(1000)
plot3d(bswissroll, col = rainbow(nrow(bswissroll)), size = 10)

# Dataset Twinpeaks ####
simuData_twinpeaks <- function(n) {
  u <-  matrix(runif(2 * n), n)
  x <- (1 - 2 * u[, 1])
  y <- sin(pi - 2 * pi * u[, 1]) * tanh(3 - 6 * u[, 2])
  z <- u[, 2]
  twinpeaks <- cbind(x, y, z)
  twinpeaks[order(y), ]
}

twinpeaks <- simuData_twinpeaks(1000)
plot3d(twinpeaks, col = rainbow(nrow(twinpeaks)), size = 10)

# Dataset Helix ####
simuData_helix <- function(n) {
  u <- matrix(runif(2 * n, 0, 100), n)
  x <- (2 + cos(8 * u[, 2])) * cos(u[, 2])
  y <- (2 + cos(8 * u[, 2])) * sin(u[, 2])
  z <- sin(8 * u[, 2])
  helix <- cbind(x, y, z)
  helix[order(x),]
}

helix <- simuData_helix(1000)
plot3d(helix, col = rainbow(nrow(helix)), size = 10)

## ESTIMATIONS ISOMAP sur Chaque jeu de donnée---------------------------------------------------------

# Estimation Swiss roll ####
SR_isomap_fit <- isomap(dist(swissroll), ndim = 2, k = 19)
plot(SR_isomap_fit$eig[1:20] / SR_isomap_fit$eig[1],
     type = 'h',
     main = "Normalized eigenvalues from Isomap")
plot(SR_isomap_fit$points, col = rainbow(nrow(swissroll)), pch = 19)

# Estimation isomap de twinpeaks
TP_isomap_fit <- isomap(dist(twinpeaks), ndim = 2, k = 19)
plot(TP_isomap_fit$eig[1:20] / TP_isomap_fit$eig[1],
     type = 'h',
     main = "Normalized eigenvalues from Isomap")
plot(TP_isomap_fit$points, col = rainbow(nrow(twinpeaks)), pch = 19)

# Estimation isomap de Broken Swiss roll
BSR_isomap_fit <- isomap(dist(bswissroll), ndim = 2, k = 13)
plot(BSR_isomap_fit$eig[1:20] / BSR_isomap_fit$eig[1],
     type = 'h',
     main = "Normalized eigenvalues from Isomap")
plot(BSR_isomap_fit$points, col = rainbow(nrow(bswissroll)), pch = 19)

# Estimation isomap de Helix
H_isomap_fit <- isomap(dist(helix), ndim = 2, k = 13)
plot(H_isomap_fit$eig[1:20] / H_isomap_fit$eig[1],
     type = 'h',
     main = "Normalized eigenvalues from Isomap")
plot(H_isomap_fit$points, col = rainbow(nrow(helix)), pch = 19)

## ESTIMATIONS LLE sur chaque jeu de données ----------------------------------------------------------
library(lle)
set.seed(24)
# Estimation LLE de Swiss Roll---------------------
# k optimal
k_SR <- calc_k(
  swissroll,
  m = 2,
  kmin = 1,
  kmax = 20,
  plotres = TRUE,
  parallel = TRUE,
  cpus = 5,
  iLLE = FALSE
)

SR_lle_fit <- lle(swissroll,
                  m = 2,
                  k = as.numeric(which.min(k_SR$rho))
                  ,
                  id = TRUE)
str(SR_lle_fit)

# plot results and intrinsic dimension (manually)

plot(
  SR_lle_fit$Y,
  main = "embedded data",
  xlab = expression(y[1]),
  ylab = expression(y[2]),
  col = rainbow(nrow(swissroll)),
  pch = 19
)

plot(
  SR_lle_fit$id,
  main = "intrinsic dimension",
  xlab = expression(x[i]),
  ylab = "id",
  col = rainbow(nrow(swissroll)),
  pch = 19
)


# Plot en  3D
plot3d(
  SR_lle_fit$Y,
  main = "embedded data",
  xlab = expression(y[1]),
  ylab = expression(y[2]),
  col = rainbow(nrow(swissroll)),
  pch = 19
)
# Estimation LLE de broken Swiss Roll ----------------
k_BSR <- calc_k(
  bswissroll,
  m = 2,
  kmin = 1,
  kmax = 20,
  plotres = TRUE,
  parallel = TRUE,
  cpus = 5,
  iLLE = FALSE
)
BSR_lle_fit <- lle(bswissroll,
                   m = 2,
                   k = as.numeric(which.min(k_BSR$rho)),
                   id = TRUE)
str(BSR_lle_fit)

# plot results and intrinsic dimension (manually)


plot(
  BSR_lle_fit$Y,
  main = "embedded data",
  xlab = expression(y[1]),
  ylab = expression(y[2]),
  col = rainbow(nrow(bswissroll)),
  pch = 19
)

plot(
  BSR_lle_fit$id,
  main = "intrinsic dimension",
  xlab = expression(x[i]),
  ylab = "id",
  col = rainbow(nrow(bswissroll)),
  pch = 19
)

# Plot en  3D
plot3d(
  BSR_lle_fit$Y,
  main = "embedded data",
  xlab = expression(y[1]),
  ylab = expression(y[2]),
  col = rainbow(nrow(bswissroll)),
  pch = 19
)

# Estimation LLE Twinpeaks ####------------------------
k_TP <- calc_k(
  twinpeaks,
  m = 2,
  kmin = 1,
  kmax = 20,
  plotres = TRUE,
  parallel = TRUE,
  cpus = 5,
  iLLE = FALSE
)
TP_lle_fit <- lle(twinpeaks,
                  m = 2,
                  k = as.numeric(which.min(k_TP$rho)),
                  id = TRUE)
str(TP_lle_fit)

# plot results and intrinsic dimension (manually)

par(mfrow = c(2, 1))
plot(
  TP_lle_fit$Y,
  main = "embedded data",
  xlab = expression(y[1]),
  ylab = expression(y[2]),
  col = rainbow(nrow(twinpeaks)),
  pch = 19
)

plot(
  TP_lle_fit$id,
  main = "intrinsic dimension",
  xlab = expression(x[i]),
  ylab = "id",
  col = rainbow(nrow(twinpeaks)),
  pch = 19
)

# Plot en  3D
plot3d(
  TP_lle_fit$Y,
  main = "embedded data",
  xlab = expression(y[1]),
  ylab = expression(y[2]),
  col = rainbow(nrow(twinpeaks)),
  pch = 19
)

# Estimation LLE Helix ----------------------------------------
k_H <- calc_k(
  helix,
  m = 2,
  kmin = 1,
  kmax = 20,
  plotres = TRUE,
  parallel = TRUE,
  cpus = 5,
  iLLE = FALSE
)
H_lle_fit <- lle(helix,
                 m = 2,
                 k = as.numeric(which.min(k_H$rho)),
                 id = TRUE)
str(H_lle_fit)

# plot results and intrinsic dimension (manually)


plot(
  H_lle_fit$Y,
  main = "embedded data",
  xlab = expression(y[1]),
  ylab = expression(y[2]),
  col = rainbow(nrow(helix)),
  pch = 19
)

plot(
  H_lle_fit$id,
  main = "intrinsic dimension",
  xlab = expression(x[i]),
  ylab = "id",
  col = rainbow(nrow(helix)),
  pch = 19
)

# Plot en  3D
plot3d(
  H_lle_fit$Y,
  main = "embedded data",
  xlab = expression(y[1]),
  ylab = expression(y[2]),
  col = rainbow(nrow(helix)),
  pch = 19
)
