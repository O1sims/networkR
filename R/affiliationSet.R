#' List the affiliation memberships of each node
#'
#' This function returns a list of the membership of each node in the hypergraph
#' @param hypergraph A dataframe of network data where nodes are in the first column and affiliations are in the second column. Nodes are members of the affiliation that they are next to.
#' @param nodeNames A dataframe where all nodes and their respective names are listed
#' @keywords affiliation projection
#' @export
#' @examples
#' affiliationSet()

affiliationSet <- function(hypergraph, nodeNames) {
  for (i in 1:nrow(nodeNames)) {
    if (i == 1) {
      affSet <- list(subset(hypergraph$affiliations,
                            hypergraph$nodes == nodeNames[i, 1]))
    } else {
      affSet[nodeNames[i, 1]] <- list(subset(hypergraph$affiliations,
                                             hypergraph$nodes == nodeNames[i, 1]))
    }
  }
  return(affSet)
}
