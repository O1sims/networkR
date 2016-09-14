#' Connectivity of a network
#'
#' This function calculates the connectivity of a network by summing the number of unique successors of each node within the network.
#' @param edgeList A dataframe of network data within which sources are in the first column and targets are in the second column.
#' @param nodeList A dataframe within which all nodes and their respective names are listed.
#' @param adjMatrix The network represented as an adjacency matrix.
#' @keywords connectivity
#' @export
#' @examples
#' connectivity()

connectivity <- function(edgeList, nodeList, adjMatrix) {
  if (missing(adjMatrix)) {
    adjMatrix <- adjacenyMatrix(edgeList,
                                nodeList)
  }
  adjMatrix[, ] <- !adjMatrix %in% c("0", "FALSE")
  for (i in 1:nrow(adjMatrix)) {
    adjMatrix <- adjMatrix + expm::`%^%`(adjMatrix, i)
    adjMatrix[, ] <- !adjMatrix %in% c("0", "FALSE")
  }
  for (i in 1:nrow(adjMatrix)) {
    adjMatrix[i, i] <- 0
  }
  k <- sum(adjMatrix)
  return(k)
}
