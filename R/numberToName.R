numberToName <- function(nodeNumbers, nodeNames) {
  translation <- ""
  translation <- sapply(1:length(nodeNumbers), function(x) {
    match <- match(nodeNumbers[x], nodeNames[, 1])
    translation[x] <- as.character(nodeNames[match, 2])
  })
  return(translation)
}