affiliationEnviornment <- function(hypergraph, affiliationNames) {
  for (i in 1:nrow(affiliationNames)) {
    members <- subset(hypergraph$nodes,
                      hypergraph$affiliations == affiliationNames[i, 1])
    if (i == 1) {
      membersList <- list(members)
    } else {
      membersList[affiliationNames[i, 1]] <- list(members)
    }
  }
  for (i in 1:nrow(affiliationNames)) {
    environment <- 0
    for (j in setdiff(seq(1, nrow(affiliationNames)), i)) {
      if (length(intersect(membersList[[i]], membersList[[j]])) > 0) {
        environment <- c(environment,
                         j)
      }
    }
    if (i == 1) {
      affEnviornment <- list(setdiff(environment, 0))
    } else {
      affEnviornment[affiliationNames[i, 1]] <- list(setdiff(environment, 0))
    }
  }
  return(affEnviornment)
}