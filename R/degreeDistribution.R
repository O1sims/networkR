degreeDistribution <- function(network, nodeNames) {
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