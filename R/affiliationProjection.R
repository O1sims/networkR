#' Represent a hypergraph as a network of affiliations
#'
#' This function projects a hypergraph into a network of affiliations.
#' @param edgeList A dataframe of network data where nodes are in the first column and affiliations are in the second column. Nodes are members of the affiliation that they are next to.
#' @keywords affiliation projection
#' @export
#' @examples
#' affiliationProjection()

affiliationProjection <- function(edgeList) {
  affiliationNetwork <- data.frame(sources = 0,
                                   targets = 0,
                                   weight = 0)
  activeAffiliations <- unique(edgeList[, 2])
  if (length(activeAffiliations) > 0) {
    for (i in 1:length(activeAffiliations)) {
      members <- subset(edgeList[, 1],
                        edgeList[, 2] == activeAffiliations[i])
      otherAffiliations <- setdiff(activeAffiliations,
                                   activeAffiliations[i])
      for (j in 1:length(otherAffiliations)) {
        overlap <- intersect(members,
                             subset(edgeList[, 1],
                                    edgeList[, 2] == otherAffiliations[j]))
        if (length(overlap) > 0) {
          affiliationNetwork <- rbind(affiliationNetwork,
                                      c(activeAffiliations[i],
                                        otherAffiliations[j],
                                        length(overlap)))
        }
      }
    }
  }
  affiliationNetwork <- affiliationNetwork[-1, ]
  return(affiliationNetwork)
}
