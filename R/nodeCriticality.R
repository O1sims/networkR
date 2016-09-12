#' Calculate the criticality of each node
#'
#' This function calculates the criticality of each node in the network. Node criticality is derived from the resulting Strong Nash equilibrium (SNE) configuration from the block formation game.
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
#' nodeCriticality()

nodeCriticality <- function(network, nodeNames, c, s, adjMatrix, setPS, setPower, approximate) {
  if (missing(s)) { c <- 0 }
  if (missing(s)) { s <- nrow(nodeNames) - 2 }
  if (missing(approximate)) { approximate <- FALSE }
  critMeasure <- setCriticality(network,
                                nodeNames,
                                c = c,
                                s = s,
                                approximate = approximate)
  nodeCriticality <- 0
  for (i in 1:nrow(nodeNames)) {
    r <- critMeasure
    t <- sapply(1:nrow(r), function(x) i %in% r$set[[x]])
    r <- r[t, ]
    if (nrow(r) > 0) {
      nodeCriticality[i] <- sum(r$criticalityMeasure)
    } else {
      nodeCriticality[i] <- 0
    }
  }
  return(nodeCriticality)
}
