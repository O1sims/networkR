#' Find the predecessors and successors of a node or node set
#'
#' This function calculates the predecessors and successors of a node or node set.
#' @param network A dataframe of network data within which sources are in the first column and targets are in the second column.
#' @param nodeNames A dataframe within which all nodes and their respective names are listed.
#' @param s The maximum size of block that is considered within the block formation game.
#' @param adjMatrix The network represented as an adjacency matrix.
#' @param approximate Should the number of sets considered be truncated? TRUE or FALSE.
#' @keywords node predecessors successors
#' @export
#' @examples
#' predecessorsSuccessors()

predecessorsSuccessors <- function(network, nodeNames, adjMatrix) {
  if (missing(adjMatrix)) {
    adjMatrix <- adjacenyMatrix(network, nodeNames)
  }
  adjMatrix[, ] <- !adjMatrix %in% c("0", "FALSE")
  for (i in 1:nrow(adjMatrix)) {
    adjMatrix <- adjMatrix + (adjMatrix %^% i)
    adjMatrix[, ] <- !adjMatrix %in% c("0", "FALSE")
  }
  for (i in 1:nrow(adjMatrix)) {
    adjMatrix[i, i] <- 0
  }
  noSuccessors <- noPredecessors <- 0
  for (i in 1:nrow(adjMatrix)) {
    noSuccessors[i] <- length(which(adjMatrix[i, ] == 1))
    noPredecessors[i] <- length(which(adjMatrix[, i] == 1))
  }
  adjMatrix <- adjacenyMatrix(network, nodeNames)
  noPredecessorsSuccessors <- data.frame(nodeNumber = seq(1, nrow(adjMatrix)),
                                         nodeName = nodeNames[, 2],
                                         noPred = noPredecessors,
                                         noSucc = noSuccessors)
  return(noPredecessorsSuccessors)
}

setPredSucc <- function(network, nodeNames, s, adjMatrix, approximate) {
  if (missing(s)) { s <- nrow(nodeNames) - 2 }
  if (s > nrow(nodeNames) - 2) {
    return(print("s must be less than or equal to number of nodes in network minus 2 [s <= nrow(nodeNames) - 2]"))
  }
  if (missing(approximate)) { approximate <- FALSE }
  if (missing(adjMatrix)) {
    adjMatrix <- adjacenyMatrix(network,
                                nodeNames)
  }
  adjMatrix[, ] <- !adjMatrix %in% c("0", "FALSE")
  for (i in 1:nrow(adjMatrix)) {
    adjMatrix <- adjMatrix + (adjMatrix %^% i)
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
    print(paste0("s = ", i, ". Analysing ", ncol(sets)," sets."))
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
        PS <- rbindlist(list(PS, a), use.names = TRUE, fill = TRUE)
      }
    }
    print(paste0("Finished s = ", i, " (Time taken ", Sys.time() - time,")"))
  }
  return(PS)
}
