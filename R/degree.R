#' Calculate node degree
#'
#' This function calculates the degree of all nodes in the network. This includes counting the in-degree and out-degree of each node. Averages of all degrees are also considered.
#' @param edgeList A dataframe of network data where sources are in the first column and targets are in the second column.
#' @param nodeList A dataframe where all nodes and their respective names are listed.
#' @keywords degree
#' @export
#' @examples
#' degree()

degree <- function(edgeList, nodeList) {
  inDeg <- outDeg <- degree <- 0
  for (i in 1:nrow(nodeList)) {
    inDeg[i] <- sum(edgeList[, 2] == i)
    outDeg[i] <- sum(edgeList[, 1] == i)
    neighbours <- c(subset(edgeList[, 2],
                           edgeList[, 1] == i),
                    subset(edgeList[, 1],
                           edgeList[, 2] == i))
    degree[i] <- length(unique(neighbours))
  }
  deg <- data.frame(name = nodeList[, 2],
                    inDegree = inDeg,
                    outDegree = outDeg,
                    degree = degree)
  return(deg)
}
