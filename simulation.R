library("rgl")
library("permute")
library("lattice")
# ACP
library("FactoMineR")
library("factoextra")
# Isomap
library("vegan")
# Plot3d
library("plot3D")
# LLE
library("lle")

rm(list = ls())
# Simulation des données

#Simulation du swissRoll
#input:
#     n: nombre de data
#     dim: nombre de dimension maniflod
#mani-tp6.r
simuData_swissRoll<-function(n,dim=2){
  u <- matrix(runif(2 * n), ncol = dim)
  v <- 3 * pi / 2 * (1 + 2 * u[, 1])
  x <- -cos(v) * v
  y <- 20 * u[, 2]
  z <- sin(v) * v
  swissroll <- cbind(x, y , z)
  layout(matrix(1:2,1:2))
  scatter3D(x, y, z, phi = 80, theta = 0)
  scatter3D(x, y, z, phi = 20, theta = 0)
  #plot3d(swissroll[order(v), ], type="p",aspect =TRUE,col=rainbow(n),size=10)
  return(swissroll[order(v), ])
}

swissroll <- simuData_swissRoll(1000)
plot3d(swissroll, type="p", aspect = TRUE, col=rainbow(nrow(swissroll)), size=10)

#Simulation du helix
#input:
#     n: nombre de data
#     dim: nombre de dimension maniflod
simuData_helix<-function(n,dim=1){
  u <- matrix(2 * pi * runif(n), ncol = dim)
  x <- (2+cos(8*u[,1]))*cos(u[,1])
  y <- (2+cos(8*u[,1]))*sin(u[,1])
  z <- sin(8*u[,1])
  helix <- cbind(x, y, z)
  layout(matrix(1:2,1:2))
  scatter3D(x, y, z, phi = 80, theta = 0)
  scatter3D(x, y, z, phi = 20, theta = 0)
  #plot3d(helix[order(x), ], type="p",aspect =TRUE,col=rainbow(n),size=10)
  return(helix[order(x), ])
}

helix <- simuData_helix(1000)
plot3d(helix, type="p", aspect = TRUE, col=rainbow(nrow(helix)), size=10)


#Simulation du sphere
#input:
#     n: nombre de data
#     r: rayon de sphere
#     dim: nombre de dimension maniflod
simuData_sphere<-function(n,r,dim=2){
  u <- matrix(runif(n*2,0,2*pi), ncol = dim)
  
  theta <- u[,1]
  phi <- u[,2]
  
  x <- (r * sin(phi) * cos(theta)) 
  y <- (r * sin(phi) * sin(theta))
  z <- (r * cos(phi))
  sphere<- cbind(x, y, z)
  layout(matrix(1:2,1:2))
  scatter3D(x, y, z, phi = 80, theta = 0)
  scatter3D(x, y, z, phi = 20, theta = 0)
  #plot3d(sphere[order(x), ], type="p",aspect =TRUE,col=rainbow(n),size=10)
  return(sphere[order(x), ])
}

sphere <- simuData_sphere(1000, r=2)
plot3d(sphere, type="p", aspect = TRUE, col=rainbow(nrow(sphere)), size=10)

#Simulation du brokenswissroll
#input:
#     n: nombre de data
#     dim: nombre de dimension maniflod
#     a,b: enlever de donnees intevalle [a,b] b>a
simuData_brokenswissroll <- function(n,dim=2, a, b) {
  # rejected an resampled
  u <- matrix(runif(2*n), ncol = dim)
  u <- u[which(u[,1]< a | u[,1]>b),]
  rows <- nrow(u)
  while (rows != n) {
    u <- rbind(u, matrix(runif(2 * (n-rows)), ncol = dim))
    u <- u[which(u[,1]< a | u[,1]>b),]
    rows <- nrow(u)
  }
  v <- 3 * pi / 2 * (1 + 2 * u[, 1])
  x <- -cos(v) * v
  y <- 20 * u[, 2]
  z <- sin(v) * v
  swissroll <- cbind(x, y , z)
  layout(matrix(1:2,1:2))
  scatter3D(x, y, z, phi = 80, theta = 0)
  scatter3D(x, y, z, phi = 20, theta = 0)
  #plot3d(swissroll[order(v), ], type="p",aspect =TRUE,col=rainbow(n),size=10)
  return(swissroll)
}

brokenswissroll <- simuData_brokenswissroll(1000, a=0.4, b=0.8)
plot3d(brokenswissroll, type="p", aspect = TRUE, col=rainbow(nrow(brokenswissroll)), size=10)

#Simulation du twinpeaks
#input:
#     n: nombre de data
#     dim: nombre de dimension maniflod
#     h: nombre de peak
simuData_twinpeaks <- function(n,dim=2,h=2) {
  # rejected an resampled
  u <- matrix(runif(2*n), ncol = dim)
  height<-h * apply(sin(h * pi * u), 1, prod)
  
  twinpeaks <- cbind(u, height)
  layout(matrix(1:2,1:2))
  scatter3D(x=u[,1], y=u[,2], z=height, phi = 80, theta = 0)
  scatter3D(x=u[,1], y=u[,2], z=height, phi = 20, theta = 0)
  #plot3d(twinpeaks, type="p",aspect =TRUE,col=rainbow(n),size=10)
  return(twinpeaks)
}

twinpeaks <- simuData_twinpeaks(1000)
plot3d(twinpeaks, type="p", aspect = TRUE, col=rainbow(nrow(twinpeaks)), size=10)

# Estimation
# ACP
res.pca2 <- prcomp(swissroll)
res.pca <- PCA(swissroll, graph=F)

fviz_pca_ind(res.pca)

plot(prcomp(swissroll)$x)

# Isomap
iso <- isomap(dist(swissroll), k=5, ndim=2)
iso <- isomap(dist(swissroll), k=13, ndim=2)
iso <- isomap(dist(swissroll), k=20, ndim=2)
str(iso)
plot(iso, pch=19)
plot(iso$points)
#plot(iso$points[order(v), ], col = rainbow(n), pch=19)
plot(iso$net)

par(mfrow=c(1,4))
iso <- isomap(dist(swissroll), k=80, ndim=2)
plot(iso, pch=19)
iso <- isomap(dist(swissroll), k=110, ndim=2)
plot(iso, pch=19)
iso <- isomap(dist(swissroll), k=150, ndim=2)
plot(iso, pch=19)
iso <- isomap(dist(swissroll), k=300, ndim=2)
plot(iso, pch=19)
par(mfrow=c(1,1))

# LLE
k50 <- lle(sphere, m=2, k=50, reg=2, ss=FALSE, id=TRUE, v=0.9 )
plot(k50$Y)
plot(k50$X)

bestk <- lle::calc_k(sphere, m=1)
res.lle <- lle(sphere, m=1, k=5)
plot(res.lle$Y)
