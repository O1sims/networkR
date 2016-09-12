coverage <- function(network, nodeNames, s, adjMatrix, setPS, perCapita, approximate) {
  if (missing(s)) {
    s <- nrow(nodeNames) - 2
  }
  if (missing(approximate)) {
    approximate <- FALSE
  }
  if (missing(perCapita)) {
    perCapita <- FALSE
  }
  if (missing(adjMatrix)) {
    originalAdjMatrix <- adjMatrix <- adjacenyMatrix(network, nodeNames)
  } else {
    originalAdjMatrix <- adjMatrix
  }
  if (missing(setPS)) {
    setPS <- setPredSucc(network, nodeNames, s, adjMatrix, approximate = approximate)
  }
  ps <- setPredSucc(network,
                    nodeNames,
                    s = 1)
  coverage <- 0
  for (i in 1:nrow(setPS)) {
    for (h in 1:length(setPS$set[[i]])) {
      if (h == 1) {
        cov <- expand.grid(c(setdiff(ps$predecessors[[setPS$set[[i]][h]]], setPS$set[[i]])), c(setdiff(ps$successors[[setPS$set[[i]][h]]], setPS$set[[i]])))
      } else {
        cov <- rbind(cov, expand.grid(c(setdiff(ps$predecessors[[setPS$set[[i]][h]]], setPS$set[[i]])), c(setdiff(ps$successors[[setPS$set[[i]][h]]], setPS$set[[i]]))))
      }
    }
    row_sub = apply(cov, 1, function(row) all(row != 0))
    cov <- cov[row_sub, ]
    if (nrow(cov) > 0) {
      for (j in 1:nrow(cov)) {
        if (cov[j, 1] == cov[j, 2]) {
          cov[j, 1] <- cov[j, 2] <- 0
        }
      }
    }
    row_sub = apply(cov, 1, function(row) all(row != 0))
    cov <- cov[row_sub, ]
    coverage[i] <- nrow(unique(cov[, ]))
  }
  setPS$coverage <- coverage
  return(setPS)
}


# Coverage measure (\gamma(B))
coverageMeasure <- function(network, nodeNames, s, adjMatrix, setPS, setPower, approximate) {
  if (missing(s)) {
    s <- nrow(nodeNames) - 2
  }
  if (missing(approximate)) {
    approximate <- FALSE
  }
  crits <- criticalSets(network,
                        nodeNames,
                        s,
                        approximate = approximate)
  critsCov <- coverage(network,
                       nodeNames,
                       s,
                       setPS = crits)
  critsCov$coverageMeasure <- round(critsCov$coverage/critsCov$setSize,
                                    digits = 3)
  return(critsCov)
}


# \overline{\gamma}_i
nodeCoverage <- function(network, nodeNames, s, adjMatrix, setPS, setPower, approximate) {
  if (missing(s)) {
    s <- nrow(nodeNames) - 2
  }
  if (missing(approximate)) {
    approximate <- FALSE
  }
  coverage <- coverageMeasure(network,
                              nodeNames,
                              s,
                              approximate = approximate)
  normaliser <- sum(coverage$coverageMeasure)
  nodeNormCoverage <- 0
  for (i in 1:nrow(nodeNames)) {
    r <- coverage
    t <- sapply(1:nrow(r), function(x) i %in% r$set[[x]])
    r <- r[t, ]
    if (nrow(r) > 0) {
      nodeNormCoverage[i] <- sum(r$coverageMeasure)
    } else {
      nodeNormCoverage[i] <- 0
    }
  }
  nodeNormCoverage <- round(nodeNormCoverage/normaliser,
                            digits = 3)
  return(nodeNormCoverage)
}