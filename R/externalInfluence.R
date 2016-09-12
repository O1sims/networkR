externalInfluence <- function(hypergraph, nodeNames, affiliationNames, weights) {
  if (missing(weights)) {
    weights <- rep(1,
                   nrow(affiliationNames))
  }
  external <- affiliationMembership <- 0
  activeAffiliations <- unique(hypergraph[, 2])
  affiliationNetwork <- affiliationProjection(hypergraph)
  for (i in 1:length(unique(hypergraph[, 2]))) {
    affiliationMembership[activeAffiliations[i]] <- length(unique(subset(hypergraph[, 1],
                                                                         hypergraph[, 2] == activeAffiliations[i])))
  }
  for (i in 1:nrow(affiliationNames)) {
    external[affiliationNames[i, 1]] <- 0
    subNetwork <- unique(subset(affiliationNetwork,
                                affiliationNetwork[, 1] == affiliationNames[i, 1]))
    neighbours <- subNetwork[, 2]

    if (length(neighbours) > 0) {
      for (j in 1:length(neighbours)) {
        external[affiliationNames[i, 1]] <- external[affiliationNames[i, 1]] + (weights[neighbours[j]] * (subNetwork[j, 3]/affiliationMembership[neighbours[j]]))
      }
    }
  }
  return(external)
}

