#' Transform network data into a data frame adjacency matrix
#'
#' This function transforms network data into an n-by-n adjacency matrix dressed up as a data frame.
#' @param edgeList A dataframe of network data where sources are in the first column and targets are in the second column.
#' @param nodeList A dataframe where all nodes and their respective names are listed.
#' @keywords adjacency matrix
#' @export
#' @examples
#' adjacencyDF()

adjacencyDF <- function(edgeList, nodeList) {
  networkDF <- adjacenyMatrix(edgeList = edgeList,
                              nodeList = nodeList)
  networkDF <- as.data.frame.matrix(networkDF)
  colnames(networkDF) <- rownames(networkDF) <- nodeList[, 2]
  return(networkDF)
}
