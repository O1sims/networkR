#' Find the predecessors and successors of a node
#'
#' This function calculates the predecessors and successors of a node or node set.
#' @param edgeList A dataframe of network data within which sources are in the first column and targets are in the second column.
#' @param nodeList A dataframe within which all nodes and their respective names are listed.
#' @param adjMatrix The network represented as an adjacency matrix.
#' @keywords node predecessors successors
#' @export
#' @examples
#' predecessorsSuccessors()

predecessorsSuccessors <- function(edgeList, nodeList, adjMatrix) {
  if (missing(adjMatrix)) {
    adjMatrix <- adjacenyMatrix(edgeList, nodeList)
  }
  adjMatrix[, ] <- !adjMatrix %in% c("0", "FALSE")
  for (i in 1:nrow(adjMatrix)) {
    adjMatrix <- adjMatrix + expm::`%^%`(adjMatrix, i)
    adjMatrix[, ] <- !adjMatrix %in% c("0", "FALSE")
  }
  for (i in 1:nrow(adjMatrix)) {
    adjMatrix[i, i] <- 0
  }
  noSuccessors <- noPredecessors <- 0
  for (i in 1:nrow(adjMatrix)) {
    noSuccessors[i] <- length(which(adjMatrix[i, ] == 1))
    noPredecessors[i] <- length(which(adjMatrix[, i] == 1))
  }
  adjMatrix <- adjacenyMatrix(edgeList, nodeList)
  noPredecessorsSuccessors <- data.frame(nodeNumber = seq(1, nrow(adjMatrix)),
                                         nodeName = nodeList[, 2],
                                         noPred = noPredecessors,
                                         noSucc = noSuccessors)
  return(noPredecessorsSuccessors)
}
