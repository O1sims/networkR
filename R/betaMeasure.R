#' Generalised $\beta$-measure (Gilles and van den Brink, 2000) applied to each node
#'
#' This function calculates the generalised $\beta$-measure, or dominance, of each node as discussed in Gilles and ven den Brink (2000). This will also consider a weighted network.
#' @param network A dataframe of network data within which sources are in the first column and targets are in the second column.
#' @param nodeNames A dataframe within which all nodes and their respective names are listed.
#' @keywords beta-measure
#' @export
#' @examples
#' betaMeasure()

betaMeasure <- function(network, nodeNames) {
  inDeg <- Beta <- 0
  for (i in 1:nrow(nodeNames)) {
    inDeg[i] <- sum(network[, 2] == i)
  }
  for (i in 1:nrow(nodeNames)) {
    Beta[i] <- 0
    successorSet <- subset(network,
                           network[, 1] == i)
    successorSet <- successorSet[, 2]

    if (length(successorSet) > 0) {
      for (j in 1:length(successorSet)) {
        Beta[i] <- Beta[i] + (1/inDeg[successorSet[j]])
      }
    }
  }
  if (sum(Beta) == sum(inDeg > 0)) {
    return(round(Beta, digits = 3))
  } else {
    return(print("We made a mistake in the calculation. HALP!"))
  }
}
