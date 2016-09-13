#' Provide detailed results of middleman power
#'
#' A function that returns detailed reults on each nodes middleman power in the network.
#' @param network A dataframe of network data where sources are in the first column and targets are in the second column.
#' @param nodeNames A dataframe where all nodes and their respective names are listed.
#' @param adjMatrix The network represented as an adjacency matrix.
#' @keywords brokerage middleman
#' @export
#' @examples
#' middlemanPowerDetail()

middlemanPowerDetail <- function(network, nodeNames, adjMatrix) {
  if (missing(adjMatrix)) {
    adjMatrix <- adjacenyMatrix(network,
                                nodeNames)
  }
  power <- middlemanPower(network,
                          nodeNames,
                          adjMatrix)
  normPower <- round(power/as.integer(potentialBrokerage(network,
                                                         nodeNames)),
                     digits = 3)
  type <- strongWeak(network,
                     nodeNames,
                     adjMatrix)
  details <- data.frame(number = nodeNames[, 1],
                        name = nodeNames[, 2],
                        power = power,
                        normPower = normPower,
                        type = type)
  return(details)
}
