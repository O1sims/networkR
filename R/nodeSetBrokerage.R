#' Brokerage of a node given its presence in a node set
#'
#' This function calculates the brokerage of each node given its existence in critical sets.
#' @param edgeList A dataframe of network data within which sources are in the first column and targets are in the second column.
#' @param nodeList A dataframe within which all nodes and their respective names are listed.
#' @param s The maximum size of block that is considered within the block formation game.
#' @param adjMatrix The network represented as an adjacency matrix.
#' @param setPS The set of predeccessors and successors for each combination of nodes considered.
#' @param perCapita The brokerage power of the node set divided by the number of nodes within the set.
#' @param approximate Should the Strong Nash Equilibrium be approximated? TRUE or FALSE.
#' @keywords brokerage
#' @export
#' @examples
#' nodeSetBrokerage()

nodeSetBrokerage <- function(edgeList, nodeList, s, perCapita, adjMatrix, setPS, setPower, approximate) {
  if (missing(s)) {
    s <- nrow(nodeList) - 2
  }
  if (missing(perCapita)) {
    perCapita <- TRUE
  }
  if (missing(approximate)) {
    approximate <- FALSE
  }
  setBrokerage <- setBrokerage(edgeList,
                               nodeList,
                               s = s,
                               perCapita = perCapita,
                               approximate = approximate)
  nodeSetBrokerage <- 0
  for (i in 1:nrow(nodeList)) {
    r <- setBrokerage
    t <- sapply(1:nrow(r), function(x) i %in% r$set[[x]])
    r <- r[t, ]
    nodeSetBrokerage[i] <- sum(r$powerCapita)
  }
  nodeSetBrokerage <- round(nodeSetBrokerage/sum(setBrokerage$powerCapita),
                            digits = 3)
  return(nodeSetBrokerage)
}
