#' Calculate the influence of nodes within a hypergraph
#'
#' The function calculates the influence of nodes and affiliations within a hypergraph. The measure accepts weights, or values, distributed over affiliations.
#' @param edgeList A dataframe of network data where nodes are in the first column and affiliations are in the second column. Nodes are members of the affiliation that they are next to.
#' @param nodeList A dataframe where all nodes and their respective names are listed.
#' @param affiliationList A dataframe where all affiliations and their respective names are listed.
#' @param weight The weight or value of an affiliation.
#' @param normalise Should the influence score be normalised? TRUE or FALSE.
#' @keywords influence
#' @export
#' @examples
#' nodeInfluence()

nodeInfluence <- function(edgeList, nodeList, affiliationList, weights, normalise) {
  if (missing(weights)) { weights <- rep(1, nrow(affiliationList)) }
  if (missing(normalise)) { normalise <- FALSE }
  sigma <- affiliationMembership <- 0
  activeAffiliations <- unique(edgeList[, 2])
  for (i in 1:length(unique(edgeList[, 2]))) {
    affiliationMembership[activeAffiliations[i]] <- length(unique(subset(edgeList[, 1],
                                                                         edgeList[, 2] == activeAffiliations[i])))
  }
  for (i in 1:nrow(nodeList)) {
    sigma[i] <- 0
    affiliations <- subset(edgeList[, 2],
                           edgeList[, 1] == nodeList[i, 1])
    for (j in 1:length(affiliations)) {
      sigma[i] <- sigma[i] + (weights[affiliations[j]]/affiliationMembership[affiliations[j]])
    }
  }
  if (normalise == TRUE) {
    sigma <- sigma/sum(weights)
  }
  return(round(sigma, digits = 3))
}
