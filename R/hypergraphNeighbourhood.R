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