#' Distinguish strong, weak, and Non-middlemen
#'
#' A function that returns whether individual nodes are strong, weak or non-middlemen
#' @param edgeList A dataframe of network data where sources are in the first column and targets are in the second column.
#' @param nodeList A dataframe where all nodes and their respective names are listed.
#' @param adjMatrix The network represented as an adjacency matrix.
#' @keywords brokerage middleman
#' @export
#' @examples
#' strongWeak()

strongWeak <- function(edgeList, nodeList, adjMatrix) {
  if (missing(adjMatrix)) {
    adjMatrix <- adjacenyMatrix(edgeList, nodeList)
  }
  power <- middlemanPower(edgeList, nodeList, adjMatrix)
  unAdjMatrix <- undirectedAdjMatrix(edgeList, nodeList, adjMatrix)
  unPower <- middlemanPower(edgeList, nodeList, unAdjMatrix)
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
