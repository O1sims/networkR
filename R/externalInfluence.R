#' Calculate the external influence of affiliations
#'
#' This function calculates the external influence of affiliations within a hypergraph. Affiliations can have values, and therefore be weighted.
#' @param edgeList A dataframe of network data where nodes are in the first column and affiliations are in the second column. Nodes are members of the affiliation that they are next to.
#' @param affiliationList A dataframe where all affiliations and their respective names are listed.
#' @param weight The weight or value of an affiliation.
#' @keywords influence
#' @export
#' @examples
#' externalInfluence()

externalInfluence <- function(edgeList, affiliationList, weight) {
  if (missing(weight)) { weight <- rep(1, nrow(affiliationList)) }
  external <- affiliationMembership <- 0
  activeAffiliations <- unique(edgeList[, 2])
  affiliationNetwork <- affiliationProjection(edgeList)
  for (i in 1:length(unique(edgeList[, 2]))) {
    affiliationMembership[activeAffiliations[i]] <- length(unique(subset(edgeList[, 1],
                                                                         edgeList[, 2] == activeAffiliations[i])))
  }
  for (i in 1:nrow(affiliationList)) {
    external[affiliationList[i, 1]] <- 0
    subNetwork <- unique(subset(affiliationNetwork,
                                affiliationNetwork[, 1] == affiliationList[i, 1]))
    neighbours <- subNetwork[, 2]

    if (length(neighbours) > 0) {
      for (j in 1:length(neighbours)) {
        external[affiliationList[i, 1]] <- external[affiliationList[i, 1]] + (weight[neighbours[j]] * (subNetwork[j, 3]/affiliationMembership[neighbours[j]]))
      }
    }
  }
  return(external)
}

