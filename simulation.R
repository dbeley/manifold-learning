# Simulation des données

# Plot3d
library("plot3D")

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

#swissroll <- simuData_swissRoll(5000)
#plot3d(swissroll, type="p", aspect = TRUE, col=rainbow(nrow(swissroll)), size=10)

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
  return(swissroll[order(v), ])
}

#brokenswissroll <- simuData_brokenswissroll(5000, a=0.4, b=0.8)
#plot3d(brokenswissroll, type="p", aspect = TRUE, col=rainbow(5000),  size=10)

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

#helix <- simuData_helix(5000)
#plot3d(helix, type="p", aspect = TRUE, col=rainbow(nrow(helix)), size=10)


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
  return(twinpeaks[order(height),])
}

#twinpeaks <- simuData_twinpeaks(5000)
#plot3d(twinpeaks, type="p", aspect = TRUE, col=rainbow(nrow(twinpeaks)), size=10)

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

#sphere <- simuData_sphere(5000, r=2)
#plot3d(sphere, type="p", aspect = TRUE, col=rainbow(nrow(sphere)), size=10)

# Données
set.seed(20)
swissroll <- simuData_swissRoll(500)
brokenswissroll <- simuData_brokenswissroll(500, a=0.4, b=0.8)
helix <- simuData_helix(500)
twinpeaks <- simuData_twinpeaks(500)
sphere <- simuData_sphere(500, r=2)

layout(matrix(1:1,1:1))