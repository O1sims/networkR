library(magrittr)


# load("~/Code/networkR/data/vbDirectors1880.rda")

dataPath <- "~/Code/networkR/data/"

vbdNodes <- data.frame(
  number = network$Individual.Id,
  nodes = network$Full.or.Forename,
  stringsAsFactors = FALSE) %>%
  dplyr::distinct()

affiliationStart <- vbdNodes %>%
  nrow() %>% `+`(1)

affiliations <- network$Director.Of %>%
  union(network$Chairman.of) %>%
  unique()

vbdAffiliations <- data.frame(
  number = seq(
    from = affiliationStart,
    to = affiliations %>%
      length() %>%
      `+`(affiliationStart)  %>% `-`(1)),
  affiliation = affiliations,
  stringsAsFactors = FALSE)

nodes <- affiliations <- c()
for (i in 1:nrow(network)) {
  nodes %<>% append(
    vbdNodes$number[
      match(
        x = network$Full.or.Forename[i] %>% as.character(),
        table = vbdNodes$nodes)])
  affiliations %<>% append(
    vbdAffiliations$number[
      match(
        x = network$Director.Of[i] %>% as.character(),
        table = vbdAffiliations$affiliation)])
}

vbdHypergraph <- data.frame(
  nodes = nodes,
  affiliations = affiliations,
  stringsAsFactors = FALSE)

save(vbdHypergraph,
     file = paste0(dataPath, "vbdHypergraph.rda"))

save(vbdNodes,
     file = paste0(dataPath, "vbdNodes.rda"))

save(vbdAffiliations,
     file = paste0(dataPath, "vbdAffiliations.rda"))


