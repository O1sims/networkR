#' Strong Nash equilibrium (SNE) configuration of a block formation game
#'
#' This function calculates the resulting SNE configuration of a block formation game along with the brokerage of the restulting sets.
#' @param network A dataframe of network data within which sources are in the first column and targets are in the second column.
#' @param nodeNames A dataframe within which all nodes and their respective names are listed.
#' @param c The cost of signalling to, and adding an, extra node to a block.
#' @param s The maximum size of block that is considered within the block formation game.
#' @param adjMatrix The network represented as an adjacency matrix.
#' @param setPS The set of predeccessors and successors for each combination of nodes considered.
#' @param perCapita The brokerage power of the node set divided by the number of nodes within the set.
#' @param approximate Should the Strong Nash Equilibrium be approximated? TRUE or FALSE.
#' @keywords brokerage
#' @export
#' @examples
#' blockSNE()

blockSNE <- function(network, nodeNames, c, s, adjMatrix, setPS, setPower, approximate) {
  if (missing(s)) { s <- nrow(nodeNames) - 2 }
  if (missing(approximate)) { approximate <- FALSE }
  if (missing(setPower)) {
    setPower <- blockPower(network,
                           nodeNames,
                           s,
                           adjMatrix,
                           setPS,
                           perCapita = TRUE,
                           approximate = approximate)
  }
  if (missing(c)) { c <- 0 }
  setPower$powerCapita <- setPower$powerCapita - (c * (setPower$setSize - 1))
  setPower <- subset(setPower,
                     setPower$powerCapita > 0)
  setPower <- setPower[order(-setPower$powerCapita), ]
  for (i in 1:(nrow(setPower) - 1)) {
    for (j in (i + 1):nrow(setPower)) {
      if (setPower$setSize[j] != 0) {
        if (TRUE %in% (setPower$set[[i]] %in% setPower$set[[j]])) {
          setPower$set[[j]] <- setPower$setSize[j] <- 0
        }
      }
    }
  }
  SNE <- setPower[!(setPower$setSize == 0), ]
  return(SNE)
}
