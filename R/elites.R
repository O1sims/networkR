#' Find all elites in the hypergraph
#'
#' This function lists all elites nodes in a hypergraph determined by some criteria.
#' @param edgeList A dataframe of network data where nodes are in the first column and affiliations are in the second column. Nodes are members of the affiliation that they are next to.
#' @param nodeList A dataframe where all node numbers and their respective names are listed.
#' @param affiliationList A dataframe where all affiliation numbers and their respective names are listed.
#' @param typeList Each affiliation is assigned a type; must be in order of affiliations.
#' @param e The number of aspects that a node exists in order for it to be determined as an elite.
#' @keywords elite
#' @export
#' @examples
#' elites()

elites <- function(edgeList, nodeList, affiliationList, typeList, e) {
  if (missing(e)) { e <- length(unique(typeList)) }
  if (e > length(unique(typeList))) {
    stop("e must be less than or equal to the number of aspects in the hypergraph.")
  }
  presence <- 0
  for (i in 1:nrow(nodeList)) {
    affiliationMembership <- subset(edgeList[, 2],
                                    edgeList[, 1] == nodeList[i, 1])
    if (length(affiliationMembership) > 0) {
      for (j in 1:length(affiliationMembership)) {
        match <- match(affiliationMembership[j],
                       affiliationList[, 1])
        t <- typeList[match]
        if (j == 1) {
          ty <- t
        } else {
          ty <- unique(c(ty, t))
        }
      }
    }
    presence[i] <- length(ty)
  }
  elites <- subset(nodeList,
                   presence >= e)
  return(elites)
}
