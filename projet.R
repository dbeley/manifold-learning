# Ancien fichier, plus utilisé

rm(list=ls())
setwd("/home/david/Nextcloud/6. Cours/Manifold Learning/Projet")

# Jeux de données implémentés
# Swiss Roll
## fonction simuData_swissroll
# Broken Swiss Roll
## fonction simuData_brokenswissroll
# Twin Peaks
## fonction simuData_twinpeaks
# Helix
## fonction simuData_helix

library("rgl")
# Swiss roll
simuData_swissroll <- function(n) {
  #n <- 1000 # Random position on the parameteric domain.
  u <- matrix(runif(2 * n), ncol = 2)
  
  v <- 3 * pi / 2 * (0.1 + 2 * u[, 1])
  
  x <- -cos(v) * v
  y <- 20 * u[, 2]
  z <- sin(v) * v
  
  swissroll <- cbind(x, y , z)
  swissroll[order(v), ]
}

swissroll <- simuData_swissroll(500)
plot3d(swissroll, col = rainbow(nrow(swissroll)), size = 10)

simuData_brokenswissroll <- function(n, a = 0, b = 10) {
  # rejected an resampled
  #n <- 1000 # Random position on the parameteric domain.
  u <- matrix(runif(2*n, 0, 100), ncol = 2)
  v <- 3 * pi / 2 * (1 + 2 * u[, 1])
  #v <- 3 * pi / 2 * (0.1 + 2 * u[, 1])
  u <- u[v > a & v < b,]
  rows <- nrow(u)
  while (rows != n) {
    u <- rbind(u, matrix(runif(2 * (n-rows), 0, 100), ncol = 2))
    v <- 3 * pi / 2 * (1 + 2 * u[, 1])
    #v <- 3 * pi / 2 * (0.1 + 2 * u[, 1])
    u <- u[v > a & v < b,]
    rows <- nrow(u)
  }
  
  v <- 3 * pi / 2 * (1 + 2 * u[, 1])
  #v <- 3 * pi / 2 * (0.1 + 2 * u[, 1])
  #x <- -cos(v) * v
  x <- cos(v) * v
  #y <- 20 * u[, 2]
  y <- 30 * u[, 2]
  z <- sin(v) * v
  swissroll <- cbind(x, y , z)
  swissroll <- swissroll[order(v), ]
  #swissroll <- swissroll[swissroll[,1] > 0.15 & swissroll[,2] > 0.5,]
  swissroll
}

bswissroll <- simuData_brokenswissroll(500, 4, 9)
plot3d(bswissroll, col = rainbow(nrow(bswissroll)), size = 10)

simuData_twinpeaks <- function(n) {
  #u <- matrix(ncol = 2, nrow = n, runif(2*n, 0, 100))
  ###x <- (1 - 2*u[,2]) + rnorm(n, 0, 1)
  #x <- (1 - 2*u[,2]) + rnorm(n, 0, 1)
  #y <- sin(pi - 2*pi*u[,2]) + rnorm(n, 0, 1)
  ##y <- sin(pi - 2*pi*u[,2]) * tanh(3 - 6*u[,1])
  #z <- tanh(3 - 6*u[,1]) + rnorm(n, 0, 1)
  #twinpeaks <- cbind(x, y) 
  #twinpeaks[order(x), ]
  
  inc <- 1.5 / sqrt(n)
  u <- matrix(ncol = 2, nrow = n, runif(2*n, 0, 100))
  x <- u[,1]
  y <- 1 - 2*pi
  z <- sin(pi - 2*pi*u[,2]) * tanh(3-6*u[,1])
  twinpeaks <- cbind(u, z)
  twinpeaks[order(z), ]
}

twinpeaks <- simuData_twinpeaks(500)
plot3d(twinpeaks, col = rainbow(nrow(twinpeaks)), size = 10)

simuData_helix <- function(n) {
  u <- matrix(ncol = 2, nrow = n, runif(2*n, 0, 100))
  x <- (2+cos(8*u[,2]))*cos(u[,2])
  y <- (2+cos(8*u[,2]))*sin(u[,2])
  z <- sin(8*u[,2])
  helix <- cbind(x, y, z)
  helix[order(x), ]
}

helix <- simuData_helix(500)
plot3d(helix, col = rainbow(nrow(helix)), size = 10)