#' Distinguish strong, weak, and Non-middlemen
#'
#' A function that returns whether individual nodes are strong, weak or non-middlemen
#' @param network A dataframe of network data where sources are in the first column and targets are in the second column.
#' @param nodeNames A dataframe where all nodes and their respective names are listed.
#' @param adjMatrix The network represented as an adjacency matrix.
#' @keywords brokerage middleman
#' @export
#' @examples
#' strongWeak()

strongWeak <- function(network, nodeNames, adjMatrix) {
  if (missing(adjMatrix)) {
    adjMatrix <- adjacenyMatrix(network, nodeNames)
  }
  power <- middlemanPower(network, nodeNames, adjMatrix)
  unAdjMatrix <- undirectedAdjMatrix(network, nodeNames, adjMatrix)
  unPower <- middlemanPower(network, nodeNames, unAdjMatrix)
  middlemanType <- sapply(1:length(power), function(x) {
    if (unPower[x] == 0 & power[x] == 0) {
      "Non-middleman"
    } else if (unPower[x] == 0 & power[x] > 0) {
      "Weak middleman"
    } else {
      "Strong middleman"
    }
  })
  return(middlemanType)
}
