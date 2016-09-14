#' Find the affiliation neighbourhood of each affiliation
#'
#' This function transforms network data into an n-by-n adjacency matrix.
#' @param edgeList A dataframe of network data where nodes are in the first column and affiliations are in the second column. Nodes are members of the affiliation that they are next to.
#' @param affiliationList A dataframe where all affiliations and their respective names are listed.
#' @keywords affiliation environment
#' @export
#' @examples
#' affiliationEnviornment()

affiliationEnviornment <- function(edgeList, affiliationList) {
  for (i in 1:nrow(affiliationList)) {
    members <- subset(edgeList$nodes,
                      edgeList$affiliations == affiliationList[i, 1])
    if (i == 1) {
      membersList <- list(members)
    } else {
      membersList[affiliationList[i, 1]] <- list(members)
    }
  }
  for (i in 1:nrow(affiliationList)) {
    environment <- 0
    for (j in setdiff(seq(1, nrow(affiliationList)), i)) {
      if (length(intersect(membersList[[i]], membersList[[j]])) > 0) {
        environment <- c(environment,
                         j)
      }
    }
    if (i == 1) {
      affEnviornment <- list(setdiff(environment, 0))
    } else {
      affEnviornment[affiliationList[i, 1]] <- list(setdiff(environment, 0))
    }
  }
  return(affEnviornment)
}
