randomGraph <- function(n, p, undirected) {
  if (n < 2) {
    return(print("You need a population of more than 1 node..."))
  }
  if (p < 0  || p > 1) {
    return(print("p can't be below zero or greater than 1..."))
  } else if (p == 0) {
    network <- data.frame(sources = NA,
                          targets = NA)
    return(network)
  } else {
    m <- max(1, n * round(n * p,
                          digits = 0))
  }
  if (missing(undirected)) {
    undirected <- FALSE
  }
  network <- data.frame(sources = round(runif(n = m,
                                              min = 1,
                                              max = n),
                                        digits = 0),
                        targets = round(runif(n = m,
                                              min = 1,
                                              max = n),
                                        digits = 0))
  network <- unique(network[,])
  for (i in 1:nrow(network)) {
    if (network$sources[i] == network$targets[i]) {
      network[i, ] <- 0
    }
  }
  row_sub <- apply(network, 1, function(row) all(row != 0))
  network <- network[row_sub, ]
  if (nrow(network) < m) {
    while (nrow(network) < m) {
      buffer <- m - nrow(network)
      addNetwork <- data.frame(sources = round(runif(n = buffer,
                                                     min = 1,
                                                     max = n),
                                               digits = 0),
                               targets = round(runif(n = buffer,
                                                     min = 1,
                                                     max = n),
                                               digits = 0))
      addNetwork <- unique(addNetwork[,])
      for (i in 1:nrow(addNetwork)) {
        if (addNetwork$sources[i] == addNetwork$targets[i]) {
          addNetwork[-i, ] <- 0
        }
      }
      if (0 %in% addNetwork$sources) {
        row_sub <- apply(addNetwork, 1, function(row) all(row != 0))
        addNetwork <- addNetwork[row_sub, ]
      }
      network <- rbind(network, addNetwork)
      network <- unique(network[,])
    }
  }
  if (undirected == TRUE) {
    unNetwork <- data.frame(sources = network$targets,
                            targets = network$sources)
    network <- rbind(network, unNetwork)
  }
  return(network)
}