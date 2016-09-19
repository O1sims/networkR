#' Calculate the robustness of a middleman position
#'
#' Robustness of a middleman measured by the number of arcs that need to be removed in order for a middleman to lose its position as a middleman.
#' @param edgeList A dataframe of network data where sources are in the first column and targets are in the second column.
#' @param nodeList A dataframe where all nodes and their respective names are listed.
#' @keywords robustness
#' @export
#' @examples
#' robustness()

robustness <- function(edgeList, nodeList) {
  robust <- rep(0, 
    nrow(nodeList))
  nodeList$power <- middlemanPower(edgeList, nodeList)
  middlemen <- middlemanSet <- subset(nodeList[, 1],
                                      nodeList$power > 0)
  connections <- as.data.frame(gtools::permutations(n = length(nodeList[, 1]),
                                                    r = 2,
                                                    v = nodeList[, 1]))
  colnames(connections) <- c("sources",
                             "targets")
  for (j in 1:nrow(connections)) {
    print(paste0(j))
    combinations <- combn(1:nrow(connections),
                          m = j)
    for (h in 1:ncol(combinations)) {
      newEdgeList <- rbind(edgeList,
                           connections[combinations[, h], ])
      newEdgeList <- unique(newEdgeList[,])
      nodeList$power <- middlemanPower(newEdgeList,
                                       nodeList)
      newMiddlemen <- subset(nodeList[, 1],
                             nodeList$power > 0)
      l <- setdiff(middlemanSet,
                   newMiddlemen)
      if (length(l) > 0) {
        for (k in 1:length(l)) { robust[l[k]] <- j }
        middlemanSet <- setdiff(middlemanSet, l)
        print(paste0("New middleman set : ", middlemanSet))
        print(cbind(nodeList[, 1], robust))
        if (length(middlemanSet) == 0) {
          return(cbind(nodeList[, 1], robust))
        }
      }
    }
  }
}
