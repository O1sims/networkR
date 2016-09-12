isUndirected <- function(network, nodeNames, adjMatrix) {
  if (missing(adjMatrix)) {
    adjMatrix <- adjacenyMatrix(network,
                                nodeNames)
  }
  return(FALSE %in% (adjMatrix == t(adjMatrix)) == FALSE)
}
