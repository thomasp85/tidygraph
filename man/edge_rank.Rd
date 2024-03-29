% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/edge_rank.R
\name{edge_rank}
\alias{edge_rank}
\alias{edge_rank_eulerian}
\title{Calculate edge ranking}
\usage{
edge_rank_eulerian(cyclic = FALSE)
}
\arguments{
\item{cyclic}{should the eulerian path start and end at the same node}
}
\value{
An integer vector giving the position of each edge in the ranking
}
\description{
This set of functions tries to calculate a ranking of the edges in a graph so
that edges sharing certain topological traits are in proximity in the
resulting order.
}
\section{Functions}{
\itemize{
\item \code{edge_rank_eulerian()}: Calculcate ranking as the visit order of a eulerian
path or cycle. If no such path or cycle exist it will return a vector of
\code{NA}s

}}
\examples{
graph <- create_notable('meredith') \%>\%
  activate(edges) \%>\%
  mutate(rank = edge_rank_eulerian())

}
