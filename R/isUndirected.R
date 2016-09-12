#' Query whether a network is undirected
#'
#' A function that queries whether some network data is undirected and returns a TRUE or FALSE value.
#' @param network A dataframe of network data where sources are in the first column and targets are in the second column.
#' @param nodeNames A dataframe where all nodes and their respective names are listed.
#' @param adjMatrix The network represented as an adjacency matrix.
#' @keywords undirected
#' @export
#' @examples
#' isUndirected()

isUndirected <- function(network, nodeNames, adjMatrix) {
  if (missing(adjMatrix)) {
    adjMatrix <- adjacenyMatrix(network,
                                nodeNames)
  }
  return(FALSE %in% (adjMatrix == t(adjMatrix)) == FALSE)
}
