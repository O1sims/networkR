# Return all critical sets of network
criticalSets <- function(network, nodeNames, s, adjMatrix, setPS, setPower, approximate) {
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
  criticalSets <- subset(setPower,
                         setPower$power > 0)
  return(criticalSets)
}