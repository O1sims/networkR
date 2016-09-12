#' Calculate node degree
#'
#' This function calculates the degree of all nodes in the network. This includes counting the in-degree and out-degree of each node.
#' @param network A dataframe of network data where sources are in the first column and targets are in the second column
#' @param nodeNames A dataframe where all nodes and their respective names are listed
#' @keywords degree
#' @export
#' @examples
#' degree()

degree <- function(network, nodeNames) {
  inDeg <- outDeg <- degree <- 0
  for (i in 1:nrow(nodeNames)) {
    inDeg[i] <- sum(network[, 2] == i)
    outDeg[i] <- sum(network[, 1] == i)
    neighbours <- c(subset(network[, 2],
                           network[, 1] == i),
                    subset(network[, 1],
                           network[, 2] == i))
    degree[i] <- length(unique(neighbours))
  }
  deg <- data.frame(name = nodeNames[, 2],
                    inDegree = inDeg,
                    outDegree = outDeg,
                    degree = degree)
  return(deg)
}