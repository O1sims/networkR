adjacenyMatrix <- function(network, nodeNames) {
  networkMatrix <- matrix(data = 0L,
                          nrow = nrow(nodeNames),
                          ncol = nrow(nodeNames))
  for (i in 1:nrow(network)) {
    networkMatrix[network[i, 1], network[i, 2]] <- networkMatrix[network[i, 1], network[i, 2]] + 1
  }
  return(networkMatrix)
}