#' Calculate average node degree in network
#'
#' This function calculates the degree of all nodes in the network. This includes counting the in-degree and out-degree of each node. Averages of all degrees are also considered.
#' @param edgeList A dataframe of network data where sources are in the first column and targets are in the second column.
#' @param nodeList A dataframe where all nodes and their respective names are listed.
#' @keywords degree
#' @export
#' @examples
#' averageDegree()

averageDegree <- function(edgeList, nodeList) {
  deg <- degree(edgeList, nodeList)
  avgDegree <- data.frame(avgInDegree = sum(deg$inDegree)/nrow(nodeList),
                          avgOutDegree = sum(deg$outDegree)/nrow(nodeList),
                          avgDegree = sum(deg$degree)/nrow(nodeList))
  return(round(avgDegree, digits = 3))
}
