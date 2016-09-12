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
