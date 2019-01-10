rank <- function(i, j, data_2d) {
  return(which(order(as.matrix(dist(data_2d))[i,])==j))
}

Uik <- function(k, i, data, data_2d) {
  #calculer k points plus proches dans data_2d mais pas dans data
  jdata <- order(as.matrix(dist(data))[i,])[2:k+1]
  jdata_2d <- order(as.matrix(dist(data_2d))[i,])[2:k+1]
  uik <- setdiff(jdata_2d, jdata)
  uik
}

trustworthiness <- function(k, data, data_2d) {
  n <- nrow(data)
  prem <- (2/(2*n - 3*k - 1))
  
  sum <- 0
  for (i in 1:n) {
    uik <- Uik(k, i, data, data_2d)
    if (length(uik) > 0) {
      for (j in 1:length(uik)) {
        sum <- sum + (rank(i, j, data_2d)-k)
        #sum <- sum + j-k
      }
    }
  }
  return (1 - (prem * sum))
}

swissroll[1:20,]

head(res.lle$X)

str(iso)
lleres <- cbind(res.lle$X, res.lle$Y)
iso$points[1:20,]

trustworthiness(2, swissroll[1:20,], iso$points[1:20,])

debug(trustworthiness)
undebug(trustworthiness)
