# SNE for block game
blockSNE <- function(network, nodeNames, c, s, adjMatrix, setPS, setPower, approximate) {
  if (missing(s)) {
    s <- nrow(nodeNames) - 2
  }
  if (missing(approximate)) {
    approximate <- FALSE
  }
  if (missing(setPower)) {
    setPower <- blockPower(network,
                           nodeNames,
                           s,
                           adjMatrix,
                           setPS,
                           perCapita = TRUE,
                           approximate = approximate)
  }
  if (missing(c)) {
    c <- 0
  }
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
