#' Brokerage of each node set within the network
#'
#' This function calculates the brokerage of each set of nodes within the network.
#' @param edgeList A dataframe of network data within which sources are in the first column and targets are in the second column.
#' @param nodeList A dataframe within which all nodes and their respective names are listed.
#' @param s The maximum size of block that is considered within the block formation game.
#' @param adjMatrix The network represented as an adjacency matrix.
#' @param setPS The set of predeccessors and successors for each combination of nodes considered.
#' @param perCapita The brokerage power of the node set divided by the number of nodes within the set.
#' @param approximate Should the Strong Nash Equilibrium be approximated? TRUE or FALSE.
#' @keywords brokerage
#' @export
#' @examples
#' setBrokerage()

setBrokerage <- function(edgeList, nodeList, s, adjMatrix, setPS, perCapita, approximate) {
  if (missing(adjMatrix)) {
    originalAdjMatrix <- adjMatrix <- adjacencyMatrix(edgeList,
                                                     nodeList)
  } else {
    originalAdjMatrix <- adjMatrix
  }
  if (missing(s)) {
    s <- nrow(nodeList) - 2
  }
  if (missing(approximate)) {
    approximate <- FALSE
  }
  if (missing(perCapita)) {
    perCapita <- FALSE
  }
  if (missing(setPS)) {
    setPS <- setPredSucc(edgeList,
                         nodeList,
                         s = s,
                         adjMatrix,
                         approximate = approximate)
  }
  K <- connectivity(adjMatrix = originalAdjMatrix)
  setPS$power <- 0
  for (i in 1:nrow(setPS)) {
    adjMatrix <- originalAdjMatrix
    K <- allSucc <- 0
    for (j in 1:nrow(nodeList)) {
      if (!(length(setPS$successors[[j]]) == 0 || setPS$successors[[j]] == 0)) {
        if (!(j %in% setPS$set[[i]])) {
          inSet <- setPS$set[[i]] %in% setPS$successors[[j]]
          noSet <- sum(inSet == TRUE)
          if (noSet > 0) {
            l <- length(setPS$successors[[j]]) - noSet + 1
            K <- K + l
          } else {
            K <- K + length(unique(setPS$successors[[j]]))
          }
        } else {
          allSucc <- c(allSucc, setPS$successors[[j]])
        }
      }
    }
    K <- K + length(setdiff(unique(allSucc), setPS$set[[i]])) - 1
    for (j in 1:length(setPS$set[[i]])) {
      adjMatrix[setPS$set[[i]][j], ] <- adjMatrix[, setPS$set[[i]][j]] <- 0
    }
    kappa <- connectivity(adjMatrix = adjMatrix)
    setPS$power[i] <- K - kappa - setPS$noSucc[[i]] - setPS$noPred[[i]]
    print(paste0("i = ", i))
  }
  potBroker <- potentialBrokerage(edgeList = edgeList, nodeList = nodeList)
  setPS$power <- round(setPS$power/1,
                       digits = 3)
  if (perCapita == TRUE) {
    setPS$powerCapita <- round(setPS$power/setPS$setSize,
                               digits = 3)
  } else {
    setPS$powerCapita <- setPS$power
  }
  return(setPS)
}
