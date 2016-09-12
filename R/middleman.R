potentialBrokerage <- function(network, nodeNames, adjMatrix) {
  d <- degree(network, nodeNames)
  PS <- predecessorsSuccessors(network, nodeNames, adjMatrix)
  potBrokerage <- 0
  for (i in 1:nrow(nodeNames)) {
    potBrokerage <- potBrokerage + (PS$noSucc[i] - d$outDegree[i])
  }
  potBrokerage <- max(potBrokerage, 1)
  return(potBrokerage)
}


middlemanPower <- function(network, nodeNames, adjMatrix, normalised) {
  if (missing(adjMatrix)) {
    originalAdjMatrix <- adjMatrix <- adjacenyMatrix(network, nodeNames)
  } else {
    originalAdjMatrix <- adjMatrix
  }
  if (missing(normalised)) {
    normalised <- FALSE
  }
  PS <- predecessorsSuccessors(network = network,
                               nodeNames = nodeNames,
                               adjMatrix = adjMatrix)
  K <- connectivity(adjMatrix = originalAdjMatrix)
  power <- 0
  for (i in 1:nrow(adjMatrix)) {
    adjMatrix <- originalAdjMatrix
    adjMatrix[i, ] <- adjMatrix[, i] <- 0
    kappa <- connectivity(adjMatrix = adjMatrix)
    power[i] <- K - kappa - PS$noPred[i] - PS$noSucc[i]
  }
  if (normalised == TRUE) {
    potBroker <- potentialBrokerage(network,
                                    nodeNames)
    power <- round(power/as.integer(potBroker),
                   digits = 3)
  }
  return(power)
}


strongWeak <- function(network, nodeNames, adjMatrix) {
  if (missing(adjMatrix)) {
    adjMatrix <- adjacenyMatrix(network, nodeNames)
  }
  power <- middlemanPower(network, nodeNames, adjMatrix)
  unAdjMatrix <- undirectedAdjMatrix(network, nodeNames, adjMatrix)
  unPower <- middlemanPower(network, nodeNames, unAdjMatrix)
  middlemanType <- sapply(1:length(power), function(x) {
    if (unPower[x] == 0 & power[x] == 0) {
      "Non-middleman"
    } else if (unPower[x] == 0 & power[x] > 0) {
      "Weak middleman"
    } else {
      "Strong middleman"
    }
  })
  return(middlemanType)
}


middlemanPowerDetail <- function(network, nodeNames, adjMatrix) {
  if (missing(adjMatrix)) {
    adjMatrix <- adjacenyMatrix(network,
                                nodeNames)
  }
  power <- middlemanPower(network,
                          nodeNames,
                          adjMatrix)
  normPower <- round(power/as.integer(potentialBrokerage(network,
                                                         nodeNames)),
                     digits = 3)
  type <- strongWeak(network,
                     nodeNames,
                     adjMatrix)
  details <- data.frame(number = nodeNames[, 1],
                        name = nodeNames[, 2],
                        power = power,
                        normPower = normPower,
                        type = type)
  return(details)
}