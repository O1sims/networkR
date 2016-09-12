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