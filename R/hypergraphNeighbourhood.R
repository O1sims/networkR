#' Find the neighbourhood of each node
#'
#' This function finds the neighbourhood of each node within a given hypergraph.
#' @param hypergraph A dataframe of network data where nodes are in the first column and affiliations are in the second column. Nodes are members of the affiliation that they are next to.
#' @param nodeNames A dataframe where all nodes and their respective names are listed.
#' @keywords neighbourhood neighbour
#' @export
#' @examples
#' hypergraphNeighbourhood()

hypergraphNeighbourhood <- function(hypergraph, nodeNames) {
  for (i in 1:nrow(nodeNames)) {
    affSet <- subset(hypergraph$affiliations,
                     hypergraph$nodes == nodeNames[i, 1])
    if (length(affSet) > 0) {
      for (j in 1:length(affSet)) {
        members <- subset(hypergraph$nodes,
                          hypergraph$affiliations == affSet[j])
        if (j == 1) {
          neighbours <- members
        } else {
          neighbours <- setdiff(union(neighbours, members),
                                nodeNames[i, 1])
        }
      }
    }
    if (i == 1) {
      neighbourhood <- list(neighbours)
    } else {
      neighbourhood[nodeNames[i, 1]] <- list(neighbours)
    }
  }
  return(neighbourhood)
}
