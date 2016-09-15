#' Density of a network
#'
#' This function calculates the density of a network graph.
#' @param edgeList A dataframe of network data within which sources are in the first column and targets are in the second column.
#' @param nodeList A dataframe within which all nodes and their respective names are listed.
#' @param undirected Is the network graph undirected? TRUE or FALSE.
#' @keywords density
#' @export
#' @examples
#' density()

density <- function(edgeList, nodeList, undirected) {
  if (missing(undirected)) { undirected <- FALSE }
  edges <- unique(edgeList[,])
  if (undirected == TRUE) {
    undirectedNet <- rbind(edges,
                           data.frame(sources = edges[, 2],
                                      targets = edges[, 1]))
    undirectedNet <- unique(undirectedNet[,])
    density <- nrow(undirectedNet) / (nrow(nodeList) * (nrow(nodeList) - 1))
    return(density)
  }
  edges <- unique(edgeList[,])
  density <- nrow(edges) / (nrow(nodeList) * (nrow(nodeList) - 1))
  return(density)
}
