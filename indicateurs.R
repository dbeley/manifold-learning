# Calcul d'indicateurs

rank <- function(i, j, data) {
  return(which(order(as.matrix(dist(data))[i,])==j))
}

Uik <- function(k, i, data, data_2d) {
  #calculer k points plus proches dans data_2d mais pas dans data
  jdata <- order(as.matrix(dist(data))[i,])[2:(k+1)]
  jdata_2d <- order(as.matrix(dist(data_2d))[i,])[2:(k+1)]
  # première version
  uik <- setdiff(jdata_2d, jdata)
  # peut-être
  #uik <- setdiff(jdata, jdata_2d)
  uik
}

trustworthiness <- function(k, data, data_2d) {
  n <- nrow(data)
  prem <- (2/(n*k*(2*n - 3*k - 1)))

  sum <- 0
  # marche pas
  #nn_k_data <- lle::find_nn_k(data, k)
  #nn_k_data_2d <- lle::find_nn_k(data_2d, k)
  for (i in 1:n) {
    # marche pas
    #uik <- setdiff(nn_k_data_2d[i,], nn_k_data[i,])
    uik <- Uik(k, i, data, data_2d)
    if (length(uik) > 0) {
      for (j in uik) {
        sum <- sum + (rank(i, j, data)-k)
      }
    }
  }
  return (1 - (prem * sum))
}

continuity <- function(k, data, data_2d) {
  n <- nrow(data)
  prem <- (2/(n*k*(2*n - 3*k - 1)))

  sum <- 0
  for (i in 1:n) {
    uik <- Uik(k, i, data_2d, data)
    if (length(uik) > 0) {
      for (j in uik) {
        sum <- sum + (rank(i, j, data_2d)-k)
      }
    }
  }
  return (1 - (prem * sum))
}

nn <- function(data, data_2d) {
  nn1 <- as.matrix(apply(as.matrix(dist(data)), 1, function(x) { order(x)[2]}))
  nn2 <- as.matrix(apply(as.matrix(dist(data_2d)), 1, function(x) { order(x)[2]}))
  length(setdiff(nn1, nn2))/nrow(data)
}
