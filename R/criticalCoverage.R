#' Per capita coverage of all critical sets
#'
#' This function calculates per capita coverage of all critical sets in the network.
#' @param network A dataframe of network data within which sources are in the first column and targets are in the second column.
#' @param nodeNames A dataframe within which all nodes and their respective names are listed.
#' @param s The maximum size of block that is considered within the block formation game.
#' @param setPS The set of predeccessors and successors for each combination of nodes considered.
#' @param approximate Should the Strong Nash Equilibrium be approximated? TRUE or FALSE.
#' @keywords coverage
#' @export
#' @examples
#' criticalCoverage()

criticalCoverage <- function(network, nodeNames, s, setPS, approximate) {
  if (missing(s)) { s <- nrow(nodeNames) - 2 }
  if (missing(approximate)) { approximate <- FALSE }
  crits <- criticalSets(network,
                        nodeNames,
                        s,
                        approximate = approximate)
  critsCov <- coverage(network,
                       nodeNames,
                       s,
                       setPS = crits)
  critsCov$coverageMeasure <- round(critsCov$coverage/critsCov$setSize,
                                    digits = 3)
  return(critsCov)
}
