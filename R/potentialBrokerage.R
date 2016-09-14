#' Potential brokerage of a network
#'
#' A function that calculates the number of indirect relationships that exist, and therefore can be potentially brokered. This acts as a normaliser for `middlemanPower`.
#' @param edgeList A dataframe of network data where sources are in the first column and targets are in the second column.
#' @param nodeList A dataframe where all nodes and their respective names are listed.
#' @param adjMatrix The network represented as an adjacency matrix.
#' @keywords brokerage middleman
#' @export
#' @examples
#' potentialBrokerage()

potentialBrokerage <- function(edgeList, nodeList, adjMatrix) {
  d <- degree(edgeList, nodeList)
  PS <- predecessorsSuccessors(edgeList, nodeList, adjMatrix)
  potBrokerage <- 0
  for (i in 1:nrow(nodeList)) {
    potBrokerage <- potBrokerage + (PS$noSucc[i] - d$outDegree[i])
  }
  potBrokerage <- max(potBrokerage, 1)
  return(potBrokerage)
}
