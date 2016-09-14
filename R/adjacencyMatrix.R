#' Transform network data into an adjacency matrix
#'
#' This function transforms network data into an n-by-n adjacency matrix.
#' @param edgeList A dataframe of network data where sources are in the first column and targets are in the second column.
#' @param nodeList A dataframe where all nodes and their respective names are listed.
#' @keywords adjacency matrix
#' @export
#' @examples
#' adjacencyMatrix()

adjacencyMatrix <- function(edgeList, nodeList) {
  networkMatrix <- matrix(data = 0L,
                          nrow = nrow(nodeList),
                          ncol = nrow(nodeList))
  for (i in 1:nrow(edgeList)) {
    networkMatrix[edgeList[i, 1], edgeList[i, 2]] <-
      networkMatrix[edgeList[i, 1], edgeList[i, 2]] + 1
  }
  return(networkMatrix)
}
