% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/affiliationSet.R
\name{affiliationSet}
\alias{affiliationSet}
\title{List the affiliation memberships of each node}
\usage{
affiliationSet(edgeList, nodeList)
}
\arguments{
\item{edgeList}{A dataframe of network data where nodes are in the first column and affiliations are in the second column. Nodes are members of the affiliation that they are next to.}

\item{nodeList}{A dataframe where all nodes and their respective names are listed.}
}
\description{
This function returns a list of the membership of each node in the hypergraph
}
\examples{
affiliationSet()
}
\keyword{affiliation}
\keyword{projection}
