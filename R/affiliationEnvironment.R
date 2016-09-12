#' Transform network data into an adjacency matrix
#'
#' This function transforms network data into an n-by-n adjacency matrix.
#' @param hypergraph A dataframe of network data where nodes are in the first column and affiliations are in the second column. Nodes are members of the affiliation that they are next to.
#' @param affiliationNames A dataframe where all affiliations and their respective names are listed.
#' @keywords affiliation environment
#' @export
#' @examples
#' affiliationEnviornment()

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
