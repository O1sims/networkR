#' Represent a hypergraph as a network of nodes only
#'
#' This function projects a hypergraph into a network of nodes only.
#' @param edgeList A dataframe of network data where nodes are in the first column and affiliations are in the second column. Nodes are members of the affiliation that they are next to.
#' @keywords node projection
#' @export
#' @examples
#' nodeProjection

nodeProjection <- function(edgeList) {
  nodeNetwork <- data.frame(sources = 0,
                            targets = 0)
  activeAffiliations <- unique(edgeList[, 2])

  if (length(activeAffiliations) > 0) {
    for (i in 1:length(activeAffiliations)) {
      affiliationSet <- subset(edgeList[, 1],
                               edgeList[, 2] == activeAffiliations[i])
      connections <- data.frame(t(combn(affiliationSet,
                                        m = 2)))
      colnames(connections) <- c("sources",
                                 "targets")
      nodeNetwork <- rbind(nodeNetwork,
                           connections,
                           data.frame(sources = connections$targets,
                                      targets = connections$sources))
    }
    nodeNetwork <- nodeNetwork[-1, ]
    return(nodeNetwork)
  } else {
    return(print("No affiliations to project!"))
  }
}
