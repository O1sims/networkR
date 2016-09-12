affiliationProjection <- function(hypergraph) {
  affiliationNetwork <- data.frame(sources = 0,
                                   targets = 0,
                                   weight = 0)
  activeAffiliations <- unique(hypergraph[, 2])

  if (length(activeAffiliations) > 0) {
    for (i in 1:length(activeAffiliations)) {
      members <- subset(hypergraph[, 1],
                        hypergraph[, 2] == activeAffiliations[i])
      otherAffiliations <- setdiff(activeAffiliations,
                                   activeAffiliations[i])
      for (j in 1:length(otherAffiliations)) {
        overlap <- intersect(members,
                             subset(hypergraph[, 1],
                                    hypergraph[, 2] == otherAffiliations[j]))
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