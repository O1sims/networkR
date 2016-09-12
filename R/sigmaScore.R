sigmaScore <- function(hypergraph, nodeNames, affiliationNames, weights) {
  if (missing(weights)) {
    weights <- rep(1, nrow(affiliationNames))
  }
  sigma <- affiliationMembership <- 0
  activeAffiliations <- unique(hypergraph[, 2])
  for (i in 1:length(unique(hypergraph[, 2]))) {
    affiliationMembership[activeAffiliations[i]] <- length(unique(subset(hypergraph[, 1],
                                                                         hypergraph[, 2] == activeAffiliations[i])))
  }
  for (i in 1:nrow(nodeNames)) {
    sigma[i] <- 0
    affiliations <- subset(hypergraph[, 2],
                           hypergraph[, 1] == nodeNames[i, 1])
    for (j in 1:length(affiliations)) {
      sigma[i] <- sigma[i] + (weights[affiliations[j]]/affiliationMembership[affiliations[j]])
    }
  }
  return(round(sigma, digits = 3))
}


normSigmaScore <- function(hypergraph, nodeNames, affiliationNames, weights) {
  if (missing(weights)) {
    weights <- rep(1,
                   nrow(affiliationNames))
  }
  sigma <- sigmaScore(hypergraph,
                      nodeNames,
                      affiliationNames,
                      weights)
  sigma <- sigma/sum(weights)
  return(sigma)
}


sigmaScoreAffiliation <- function(hypergraph, nodeNames, affiliationNames, weights) {
  if (missing(weights)) {
    weights <- rep(1,
                   max(affiliationNames$number))
  }
  sigma <- affiliationMembership <- 0
  activeAffiliations <- unique(hypergraph[, 2])
  affiliationNetwork <- affiliationProjection(hypergraph)
  for (i in 1:length(unique(hypergraph[, 2]))) {
    affiliationMembership[activeAffiliations[i]] <- length(unique(subset(hypergraph[, 1],
                                                                         hypergraph[, 2] == activeAffiliations[i])))
  }
  for (i in 1:nrow(affiliationNames)) {
    sigma[affiliationNames[i, 1]] <- weights[affiliationNames[i, 1]]
    subNetwork <- unique(subset(affiliationNetwork,
                                affiliationNetwork[, 1] == affiliationNames[i, 1]))
    if (nrow(subNetwork) > 0) {
      neighbours <- subNetwork[, 2]
      for (j in 1:length(neighbours)) {
        sigma[affiliationNames[i, 1]] <-
          sigma[affiliationNames[i, 1]] + (weights[neighbours[j]] * (subNetwork[j, 3]/max(affiliationMembership[neighbours[j]], 1)))
      }
    }
  }
  return(sigma)
}