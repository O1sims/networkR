#' Calculate the criticality of all node sets in Strong Nash equilibrium (SNE)
#'
#' This function calculates the criticality of all node sets that remain in all resulting SNE configurations of a block formation game.
#' @param network A dataframe of network data within which sources are in the first column and targets are in the second column.
#' @param nodeNames A dataframe within which all nodes and their respective names are listed.
#' @param c The cost of signalling to, and adding an, extra node to a block.
#' @param s The maximum size of block that is considered within the block formation game.
#' @param adjMatrix The network represented as an adjacency matrix.
#' @param setPS The set of predeccessors and successors for each combination of nodes considered.
#' @param approximate Should the Strong Nash Equilibrium be approximated? TRUE or FALSE.
#' @keywords criticality
#' @export
#' @examples
#' setCriticality()

setCriticality <- function(network, nodeNames, c, s, adjMatrix, setPS, setPower, approximate) {
  if (missing(c)) { c <- 0 }
  if (missing(s)) { s <- nrow(nodeNames) - 2 }
  if (missing(approximate)) {
    approximate <- FALSE
  }
  critMeasure <- blockSNE(network,
                          nodeNames,
                          c = c,
                          s = s,
                          approximate = approximate)
  colnames(critMeasure) <- c("set", "setSize", "successors", "predecessors", "noSucc",
                             "noPred", "criticality", "criticalityMeasure")
  return(critMeasure)
}
