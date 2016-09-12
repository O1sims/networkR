#' Calculate the external influence of affiliations
#'
#' This function calculates the external influence of affiliations within a hypergraph. Affiliations can have values, and therefore be weighted.
#' @param hypergraph A dataframe of network data where nodes are in the first column and affiliations are in the second column. Nodes are members of the affiliation that they are next to.
#' @param nodeNames A dataframe where all nodes and their respective names are listed.
#' @param affiliationNames A dataframe where all affiliations and their respective names are listed.
#' @param weight The weight or value of an affiliation.
#' @keywords influence
#' @export
#' @examples
#' externalInfluence()

externalInfluence <- function(hypergraph, nodeNames, affiliationNames, weight) {
  if (missing(weight)) { weight <- rep(1, nrow(affiliationNames)) }
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
        external[affiliationNames[i, 1]] <- external[affiliationNames[i, 1]] + (weight[neighbours[j]] * (subNetwork[j, 3]/affiliationMembership[neighbours[j]]))
      }
    }
  }
  return(external)
}

