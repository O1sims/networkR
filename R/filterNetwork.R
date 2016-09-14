#' Filter a networks edges
#'
#' This function filters a network such that it removes any duplicated connections, or more specifically arcs, between a pair of nodes.
#' @param edgeList A dataframe of network data where sources are in the first column and targets are in the second column.
#' @keywords filter
#' @export
#' @examples
#' filterNetwork()

filterNetwork <- function(edgeList) {
  for (i in 1:(nrow(edgeList) - 1)) {
    for (j in (i + 1):nrow(edgeList)) {
      if (edgeList$targets[i] == edgeList$sources[j] &&
          edgeList$sources[i] == edgeList$targets[j]) {
        edgeList[i, ] <- 0
      }
    }
  }
  row_sub <- apply(edgeList, 1, function(row) all(row != 0))
  edgeList <- edgeList[row_sub, ]
  return(edgeList)
}
