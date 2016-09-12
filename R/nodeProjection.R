nodeProjection <- function(hypergraph) {
  nodeNetwork <- data.frame(sources = 0,
                            targets = 0)
  activeAffiliations <- unique(hypergraph[, 2])

  if (length(activeAffiliations) > 0) {
    for (i in 1:length(activeAffiliations)) {
      affiliationSet <- subset(hypergraph[, 1],
                               hypergraph[, 2] == activeAffiliations[i])
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