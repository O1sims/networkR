#' Plot degree distribution
#'
#' This function plots the degree distribution of a network. Depends on `ggplot2`.
#' @param edgeList A dataframe of network data where sources are in the first column and targets are in the second column.
#' @param nodeList A dataframe where all nodes and their respective names are listed.
#' @keywords degree distribution
#' @export
#' @examples
#' degreeDistribution()

degreeDistribution <- function(edgeList, nodeList) {
  deg <- degree(edgeList, nodeList)
  degDistPlot <- ggplot2::ggplot(deg,
                                 aes(x = deg$degree)) +
    geom_histogram(binwidth = 0.5,
                   alpha = 0.8) +
    labs(title = "Degree distribution",
         x = "Degree",
         y = "Count") +
    theme(legend.position = "none")
  return(degDistPlot)
}
