undirectedAdjMatrix <- function(network, nodeNames, adjMatrix) {
  if (missing(adjMatrix)) {
    adjMatrix <- adjacenyMatrix(network,
                                nodeNames)
  }
  adjMatrix <- adjMatrix + t(adjMatrix)
  adjMatrix[, ] <- !adjMatrix %in% c("0", "FALSE")
  return(adjMatrix)
}
