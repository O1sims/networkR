#' Transform network data into a data frame adjacency matrix
#'
#' This function transforms network data into an n-by-n adjacency matrix dressed up as a data frame.
#' @param network A dataframe of network data where sources are in the first column and targets are in the second column.
#' @param nodeNames A dataframe where all nodes and their respective names are listed.
#' @keywords adjacency matrix
#' @export
#' @examples
#' adjacencyDF()

adjacencyDF <- function(network, nodeNames) {
  networkDF <- adjacenyMatrix(network = network,
                              nodeNames = nodeNames)
  networkDF <- as.data.frame.matrix(networkDF)
  colnames(networkDF) <- rownames(networkDF) <- nodeNames[, 2]
  return(networkDF)
}
