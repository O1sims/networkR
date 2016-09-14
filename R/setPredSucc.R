#' Find the predecessors and successors of a node set
#'
#' This function calculates the predecessors and successors of a node or node set.
#' @param edgeList A dataframe of network data within which sources are in the first column and targets are in the second column.
#' @param nodeList A dataframe within which all nodes and their respective names are listed.
#' @param s The maximum size of block that is considered within the block formation game.
#' @param adjMatrix The network represented as an adjacency matrix.
#' @param approximate Should the number of sets considered be truncated? TRUE or FALSE.
#' @keywords node predecessors successors
#' @export
#' @examples
#' setPredSucc()

setPredSucc <- function(edgeList, nodeList, s, adjMatrix, approximate) {
  if (missing(s)) { s <- nrow(nodeList) - 2 }
  if (s > nrow(nodeList) - 2) {
    return(print("s must be less than or equal to number of nodes in network minus 2 [s <= nrow(nodeList) - 2]"))
  }
  if (missing(approximate)) { approximate <- FALSE }
  if (missing(adjMatrix)) {
    adjMatrix <- adjacencyMatrix(edgeList,
                                nodeList)
  }
  adjMatrix[, ] <- !adjMatrix %in% c("0", "FALSE")
  for (i in 1:nrow(adjMatrix)) {
    adjMatrix <- adjMatrix + expm::`%^%`(adjMatrix, i)
    adjMatrix[, ] <- !adjMatrix %in% c("0", "FALSE")
  }
  for (i in 1:nrow(adjMatrix)) {
    adjMatrix[i, i] <- 0
  }
  time <- Sys.time()
  for (i in 1:s) {
    sets <- combn(seq(1:nrow(adjMatrix)),
                  m = i)
    if (approximate == TRUE) {
      if (ncol(sets) > 500000) {
        sets <- sets[, sample(ncol(sets))]
        sets <- sets[, sample(1:ncol(sets),
                              size = 500000,
                              replace = FALSE)]
      }
    }
    for (j in 1:ncol(sets)) {
      set <- sets[, j]
      successors <- predecessors <- noSucc <- noPred <- 0
      for (k in 1:i) {
        successors <- c(successors,
                        which(adjMatrix[set[k], ] == 1))
        predecessors <- c(predecessors,
                          which(adjMatrix[, set[k]] == 1))
      }
      if (length(successors) > 1) {
        successors <- setdiff(unique(successors),
                              c(set, 0))
        noSucc <- length(successors)
      } else {
        successors <- noSucc <- 0
      }
      if (length(predecessors) > 1) {
        predecessors <- setdiff(unique(predecessors),
                                c(set, 0))
        noPred <- length(predecessors)
      } else {
        predecessors <- noPred <- 0
      }
      if (i == 1 & j == 1 & k == 1) {
        PS <- list(set = list(set),
                   setSize = i,
                   successors = list(successors),
                   predecessors = list(predecessors),
                   noSucc = list(noSucc),
                   noPred = list(noPred))
      } else {
        a <- list(set = list(set),
                  setSize = i,
                  successors = list(successors),
                  predecessors = list(predecessors),
                  noSucc = list(noSucc),
                  noPred = list(noPred))
        PS <- data.table::rbindlist(list(PS, a),
                                    use.names = TRUE,
                                    fill = TRUE)
      }
    }
  }
  return(PS)
}
