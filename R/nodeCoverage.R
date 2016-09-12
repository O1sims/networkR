#' Calculate the coverage of each node
#'
#' This function calculates the coverage for each node based on their existence within blocks.
#' @param network A dataframe of network data within which sources are in the first column and targets are in the second column.
#' @param nodeNames A dataframe within which all nodes and their respective names are listed.
#' @param s The maximum size of block that is considered within the block formation game.
#' @param adjMatrix The network represented as an adjacency matrix.
#' @param setPS The set of predeccessors and successors for each combination of nodes considered.
#' @param approximate Should the Strong Nash Equilibrium be approximated? TRUE or FALSE.
#' @keywords node coverage
#' @export
#' @examples
#' nodeCoverage()

nodeCoverage <- function(network, nodeNames, s, adjMatrix, setPS, approximate) {
  if (missing(s)) { s <- nrow(nodeNames) - 2 }
  if (missing(approximate)) { approximate <- FALSE }
  coverage <- criticalCoverage(network,
                              nodeNames,
                              s,
                              approximate = approximate)
  normaliser <- sum(coverage$criticalCoverage)
  nodeNormCoverage <- 0
  for (i in 1:nrow(nodeNames)) {
    r <- coverage
    t <- sapply(1:nrow(r), function(x) i %in% r$set[[x]])
    r <- r[t, ]
    if (nrow(r) > 0) {
      nodeNormCoverage[i] <- sum(r$criticalCoverage)
    } else {
      nodeNormCoverage[i] <- 0
    }
  }
  nodeNormCoverage <- round(nodeNormCoverage/normaliser,
                            digits = 3)
  return(nodeNormCoverage)
}
