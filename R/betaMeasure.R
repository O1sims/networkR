# \beta_{i}(D) Beta measure (Gilles and van den Brink)
# This is a generalised measure and will take in a weighted network
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