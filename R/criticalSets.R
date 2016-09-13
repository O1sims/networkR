#' Find all critical sets in a network
#'
#' This function calculates all critical sets in the network.
#' @param network A dataframe of network data within which sources are in the first column and targets are in the second column.
#' @param nodeNames A dataframe within which all nodes and their respective names are listed.
#' @param s The maximum size of block that is considered within the block formation game.
#' @param adjMatrix The network represented as an adjacency matrix.
#' @param setPS The set of predeccessors and successors for each combination of nodes considered.
#' @param setPower All sets within a network along with their respective brokerage scores.
#' @param approximate Should the Strong Nash Equilibrium be approximated? TRUE or FALSE.
#' @keywords critical sets
#' @export
#' @examples
#' criticalSets()

criticalSets <- function(network, nodeNames, s, adjMatrix, setPS, setPower, approximate) {
  if (missing(s)) { s <- nrow(nodeNames) - 2 }
  if (missing(approximate)) { approximate <- FALSE }
  if (missing(setPower)) {
    setPower <- setBrokerage(network,
                             nodeNames,
                             s = s,
                             adjMatrix,
                             setPS,
                             perCapita = FALSE,
                             approximate = approximate)
  }
  criticalSets <- subset(setPower,
                         setPower$power > 0)
  return(criticalSets)
}
