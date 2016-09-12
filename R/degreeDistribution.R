#' Plot degree distribution
#'
#' This function plots the degree distribution of a network. Depends on `ggplot2`.
#' @param network A dataframe of network data where sources are in the first column and targets are in the second column.
#' @param nodeNames A dataframe where all nodes and their respective names are listed.
#' @keywords degree distribution
#' @export
#' @examples
#' degreeDistribution()

degreeDistribution <- function(network, nodeNames) {
  library(ggplot2)
  deg <- degree(network, nodeNames)
  degDistPlot <- ggplot(deg,
                        aes(x = deg$degree)) +
    geom_histogram(binwidth = 0.5,
                   alpha = 0.8) +
    labs(title = "Degree distribution",
         x = "Degree",
         y = "Count") +
    theme(legend.position = "none")
  return(degDistPlot)
}
