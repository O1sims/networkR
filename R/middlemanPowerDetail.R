#' Provide detailed results of middleman power fo all nodes
#'
#' A function that returns detailed reults on each nodes middleman power in the network.
#' @param network A dataframe of network data where sources are in the first column and targets are in the second column.
#' @param nodeList A dataframe where all nodes and their respective names are listed.
#' @param adjMatrix The network represented as an adjacency matrix.
#' @keywords brokerage middleman
#' @export
#' @examples
#' middlemanPowerDetail()

middlemanPowerDetail <- function(edgeList, nodeList, adjMatrix) {
  if (missing(adjMatrix)) {
    adjMatrix <- adjacenyMatrix(edgeList,
                                nodeList)
  }
  power <- middlemanPower(edgeList,
                          nodeList,
                          adjMatrix)
  normPower <- round(power/as.integer(potentialBrokerage(edgeList,
                                                         nodeList)),
                     digits = 3)
  type <- strongWeak(edgeList,
                     nodeList,
                     adjMatrix)
  details <- data.frame(number = nodeList[, 1],
                        name = nodeList[, 2],
                        power = power,
                        normPower = normPower,
                        type = type)
  return(details)
}
