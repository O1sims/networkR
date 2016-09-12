#' Calculate Middleman power
#'
#' A function that calculates the middleman power of each node in a network.
#' @param network A dataframe of network data where sources are in the first column and targets are in the second column.
#' @param nodeNames A dataframe where all nodes and their respective names are listed.
#' @param adjMatrix The network represented as an adjacency matrix.
#' @keywords brokerage middleman
#' @export
#' @examples
#' middlemanPower()

middlemanPower <- function(network, nodeNames, adjMatrix, normalised) {
  if (missing(adjMatrix)) {
    originalAdjMatrix <- adjMatrix <- adjacenyMatrix(network, nodeNames)
  } else {
    originalAdjMatrix <- adjMatrix
  }
  if (missing(normalised)) { normalised <- FALSE }
  PS <- predecessorsSuccessors(network = network,
                               nodeNames = nodeNames,
                               adjMatrix = adjMatrix)
  K <- connectivity(adjMatrix = originalAdjMatrix)
  power <- 0
  for (i in 1:nrow(adjMatrix)) {
    adjMatrix <- originalAdjMatrix
    adjMatrix[i, ] <- adjMatrix[, i] <- 0
    kappa <- connectivity(adjMatrix = adjMatrix)
    power[i] <- K - kappa - PS$noPred[i] - PS$noSucc[i]
  }
  if (normalised == TRUE) {
    potBroker <- potentialBrokerage(network,
                                    nodeNames,
                                    adjMatrix = originalAdjMatrix)
    power <- round(power/as.integer(potBroker),
                   digits = 3)
  }
  return(power)
}
