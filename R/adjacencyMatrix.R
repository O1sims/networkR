#' Transform network data into an adjacency matrix
#'
#' This function transforms network data into an n-by-n adjacency matrix.
#' @param network A dataframe of network data where sources are in the first column and targets are in the second column.
#' @param nodeNames A dataframe where all nodes and their respective names are listed.
#' @keywords adjacency matrix
#' @export
#' @examples
#' adjacencyMatrix()

adjacenyMatrix <- function(network, nodeNames) {
  networkMatrix <- matrix(data = 0L,
                          nrow = nrow(nodeNames),
                          ncol = nrow(nodeNames))
  for (i in 1:nrow(network)) {
    networkMatrix[network[i, 1], network[i, 2]] <-
      networkMatrix[network[i, 1], network[i, 2]] + 1
  }
  return(networkMatrix)
}
