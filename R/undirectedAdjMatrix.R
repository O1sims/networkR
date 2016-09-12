#' Coerce adjacency matrix to be undirected
#'
#' A function that coerces an adjacency matrix to become undirected.
#' @param network A dataframe of network data where sources are in the first column and targets are in the second column.
#' @param nodeNames A dataframe where all nodes and their respective names are listed.
#' @param adjMatrix The network represented as an adjacency matrix.
#' @keywords undirected
#' @export
#' @examples
#' undirectedAdjMatrix()

undirectedAdjMatrix <- function(network, nodeNames, adjMatrix) {
  if (missing(adjMatrix)) {
    adjMatrix <- adjacenyMatrix(network,
                                nodeNames)
  }
  adjMatrix <- adjMatrix + t(adjMatrix)
  adjMatrix[, ] <- !adjMatrix %in% c("0", "FALSE")
  return(adjMatrix)
}
