source("simulation.R")

#function isomap
#input:
#     data: matrice de donnees
#     k: nombre de dimension que on veut obtenir
esti_isomap<-function(data,k){
  
}



sphere<-simuData_sphere(1000,0.5)

swis<-simuData_swissRoll(1000)
distances<- dist(swis,p=2)
x_iso <- isomap(distances, ndim=2, k=5)
plot(x_iso$points, col = jet.col(1000), xlab="", ylab="", cex.axis=1.5)


