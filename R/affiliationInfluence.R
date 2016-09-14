#' Calculate the influence of affiliations within a hypergraph
#'
#' The function calculates the influence of nodes and affiliations within a hypergraph. The measure accepts weights, or values, distributed over affiliations.
#' @param edgeList A dataframe of network data where nodes are in the first column and affiliations are in the second column. Nodes are members of the affiliation that they are next to.
#' @param affiliationList A dataframe where all affiliations and their respective names are listed.
#' @param weight The weight or value of an affiliation.
#' @param normalise Should the influence score be normalised? TRUE or FALSE.
#' @keywords influence
#' @export
#' @examples
#' affiliationInfluence()

affiliationInfluence <- function(edgeList, affiliationList, weights, normalise) {
  if (missing(weights)) { weights <- rep(1, max(affiliationList$number)) }
  if (missing(normalise)) { normalise <- FALSE }
  sigma <- affiliationMembership <- 0
  activeAffiliations <- unique(edgeList[, 2])
  affiliationNetwork <- affiliationProjection(edgeList)
  for (i in 1:length(unique(edgeList[, 2]))) {
    affiliationMembership[activeAffiliations[i]] <- length(unique(subset(edgeList[, 1],
                                                                         edgeList[, 2] == activeAffiliations[i])))
  }
  for (i in 1:nrow(affiliationList)) {
    sigma[affiliationList[i, 1]] <- weights[affiliationList[i, 1]]
    subNetwork <- unique(subset(affiliationNetwork,
                                affiliationNetwork[, 1] == affiliationList[i, 1]))
    if (nrow(subNetwork) > 0) {
      neighbours <- subNetwork[, 2]
      for (j in 1:length(neighbours)) {
        sigma[affiliationList[i, 1]] <-
          sigma[affiliationList[i, 1]] + (weights[neighbours[j]] * (subNetwork[j, 3]/max(affiliationMembership[neighbours[j]], 1)))
      }
    }
  }
  if (normalise == TRUE) {
    sigma <- sigma/sum(weights)
  }
  return(sigma)
}
