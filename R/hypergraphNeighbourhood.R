#' Find the neighbourhood of each node
#'
#' This function finds the neighbourhood of each node within a given hypergraph.
#' @param edgeList A dataframe of network data where nodes are in the first column and affiliations are in the second column. Nodes are members of the affiliation that they are next to.
#' @param nodeList A dataframe where all nodes and their respective names are listed.
#' @keywords neighbourhood neighbour
#' @export
#' @examples
#' hypergraphNeighbourhood()

hypergraphNeighbourhood <- function(edgeList, nodeList) {
  for (i in 1:nrow(nodeList)) {
    affSet <- subset(edgeList$affiliations,
                     edgeList$nodes == nodeList[i, 1])
    if (length(affSet) > 0) {
      for (j in 1:length(affSet)) {
        members <- subset(edgeList$nodes,
                          edgeList$affiliations == affSet[j])
        if (j == 1) {
          neighbours <- setdiff(members,
                                nodeList[i, 1])
        } else {
          neighbours <- setdiff(union(neighbours, members),
                                nodeList[i, 1])
        }
      }
    }
    if (i == 1) {
      neighbourhood <- list(neighbours)
    } else {
      neighbourhood[nodeList[i, 1]] <- list(neighbours)
    }
  }
  return(neighbourhood)
}
