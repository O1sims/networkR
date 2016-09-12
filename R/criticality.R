

# Criticality Measure (\rho(B))
criticalityMeasure <- function(network, nodeNames, s, adjMatrix, setPS, setPower, approximate) {
  if (missing(s)) {
    s <- nrow(nodeNames) - 2
  }
  if (missing(approximate)) {
    approximate <- FALSE
  }
  critMeasure <- blockSNE(network,
                          nodeNames,
                          s = s,
                          approximate = approximate)
  colnames(critMeasure) <- c("set", "setSize", "successors", "predecessors", "noSucc",
                             "noPred", "criticality", "criticalityMeasure")
  return(critMeasure)
}


# \widehat{\rho}_i
nodeCriticality <- function(network, nodeNames, s, adjMatrix, setPS, setPower, approximate) {
  if (missing(s)) {
    s <- nrow(nodeNames) - 2
  }
  if (missing(approximate)) {
    approximate <- FALSE
  }
  critMeasure <- criticalityMeasure(network,
                                    nodeNames,
                                    s = s,
                                    approximate = approximate)
  nodeCriticality <- 0
  for (i in 1:nrow(nodeNames)) {
    r <- critMeasure
    t <- sapply(1:nrow(r), function(x) i %in% r$set[[x]])
    r <- r[t, ]
    if (nrow(r) > 0) {
      nodeCriticality[i] <- sum(r$criticalityMeasure)
    } else {
      nodeCriticality[i] <- 0
    }
  }
  return(nodeCriticality)
}


# \overline{\rho}_i
nodeNormCriticality <- function(network, nodeNames, s, adjMatrix, setPS, setPower, approximate) {
  if (missing(s)) {
    s <- nrow(nodeNames) - 2
  }
  if (missing(approximate)) {
    approximate <- FALSE
  }
  critMeasure <- criticalityMeasure(network,
                                    nodeNames,
                                    s = s,
                                    approximate = approximate)
  normaliser <- sum(critMeasure$criticalityMeasure)
  nodeNormCriticality <- 0
  for (i in 1:nrow(nodeNames)) {
    r <- critMeasure
    t <- sapply(1:nrow(r), function(x) i %in% r$set[[x]])
    r <- r[t, ]
    if (nrow(r) > 0) {
      nodeNormCriticality[i] <- sum(r$criticalityMeasure)
    } else {
      nodeNormCriticality[i] <- 0
    }
  }
  nodeNormCriticality <- round(nodeNormCriticality/normaliser,
                               digits = 3)
  return(nodeNormCriticality)
}