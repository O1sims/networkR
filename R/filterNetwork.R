#' Filter network
#'
#' This function filters a network such that it removes any duplicated connections, or more specifically arcs, between a pair of nodes.
#' @param network A dataframe of network data where sources are in the first column and targets are in the second column.
#' @keywords filter
#' @export
#' @examples
#' filterNetwork()

filterNetwork <- function(network) {
  for (i in 1:(nrow(network) - 1)) {
    for (j in (i + 1):nrow(network)) {
      if (network$targets[i] == network$sources[j] &&
          network$sources[i] == network$targets[j]) {
        network[i, ] <- 0
      }
    }
  }
  row_sub <- apply(network, 1, function(row) all(row != 0))
  network <- network[row_sub, ]
  return(network)
}
