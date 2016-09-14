#' Calculate the coverage of all sets of nodes
#'
#' This function calculates the coverage, i.e., the number of indirect connections that the node set faciliates, for each set of nodes in the network.
#' @param edgeList A dataframe of network data within which sources are in the first column and targets are in the second column.
#' @param nodeList A dataframe within which all nodes and their respective names are listed.
#' @param s The maximum size of block that is considered within the block formation game.
#' @param adjMatrix The network represented as an adjacency matrix.
#' @param setPS The set of predeccessors and successors for each combination of nodes considered.
#' @param approximate Should the Strong Nash Equilibrium be approximated? TRUE or FALSE.
#' @keywords coverage
#' @export
#' @examples
#' setCoverage()

setCoverage <- function(edgeList, nodeList, s, adjMatrix, setPS, approximate) {
  if (missing(s)) { s <- nrow(nodeList) - 2 }
  if (missing(approximate)) { approximate <- FALSE }
  if (missing(adjMatrix)) {
    originalAdjMatrix <- adjMatrix <- adjacencyMatrix(edgeList,
                                                     nodeList)
  } else {
    originalAdjMatrix <- adjMatrix
  }
  if (missing(setPS)) { setPS <- setPredSucc(edgeList, nodeList, s, adjMatrix, approximate = approximate) }
  ps <- setPredSucc(edgeList,
                    nodeList,
                    s = 1)
  coverage <- 0
  for (i in 1:nrow(setPS)) {
    for (h in 1:length(setPS$set[[i]])) {
      if (h == 1) {
        cov <- expand.grid(c(setdiff(ps$predecessors[[setPS$set[[i]][h]]], setPS$set[[i]])),
                           c(setdiff(ps$successors[[setPS$set[[i]][h]]], setPS$set[[i]])))
      } else {
        cov <- rbind(cov,
                     expand.grid(c(setdiff(ps$predecessors[[setPS$set[[i]][h]]], setPS$set[[i]])),
                                 c(setdiff(ps$successors[[setPS$set[[i]][h]]], setPS$set[[i]]))))
      }
    }
    row_sub = apply(cov, 1, function(row) all(row != 0))
    cov <- cov[row_sub, ]
    if (nrow(cov) > 0) {
      for (j in 1:nrow(cov)) {
        if (cov[j, 1] == cov[j, 2]) {
          cov[j, 1] <- cov[j, 2] <- 0
        }
      }
    }
    row_sub = apply(cov, 1, function(row) all(row != 0))
    cov <- cov[row_sub, ]
    coverage[i] <- nrow(unique(cov[, ]))
  }
  setPS$coverage <- coverage
  return(setPS)
}
