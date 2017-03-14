#' Calculate node centrality
#'
#' The centrality of a node measures the importance of node in the network. As
#' the concept of importance is ill-defined and dependent on the network and
#' the questions under consideration, many centrality measures exist.
#' `tidygraph` provides a consistent set of wrappers for all the centrality
#' measures implemented in `igraph` for use inside [dplyr::mutate()] and other
#' relevant verbs. All functions provided by `tidygraph` have a consistent
#' naming scheme and automatically calls the function on the graph, returning a
#' vector with measures ready to be added to the node data.
#'
#' @param ... Parameters passed on to the `igraph` implementation.
#'
#' @return A numeric vector giving the centrality measure of each node.
#'
#' @name centrality
#' @rdname centrality
#'
#' @examples
#' as_tbl_graph(igraph::make_graph('bull')) %>%
#'   activate(nodes) %>%
#'   mutate(importance = centrality_alpha())
NULL

#' @describeIn centrality Wrapper for [igraph::alpha_centrality()]
#' @importFrom igraph V alpha_centrality
#' @export
centrality_alpha <- function(...) {
  graph <- .G()
  alpha_centrality(graph = graph, nodes = V(graph), ...)
}
#' @describeIn centrality Wrapper for [igraph::authority_score()]
#' @importFrom igraph authority_score
#' @export
centrality_authority <- function(...) {
  authority_score(graph = .G(), ...)$vector
}
#' @describeIn centrality Wrapper for [igraph::betweenness()] and [igraph::estimate_betweenness()]
#' @importFrom igraph V betweenness estimate_betweenness
#' @importFrom rlang tidy_quotes
#' @export
centrality_betweenness <- function(...) {
  graph <- .G()
  dots <- tidy_quotes(...)
  if (is.null(dots$cutoff)) {
    betweenness(graph = graph, v = V(graph), ...)
  } else {
    estimate_betweenness(graph = graph, vids = V(graph), ...)
  }
}
#' @describeIn centrality Wrapper for [igraph::power_centrality()]
#' @importFrom igraph V power_centrality
#' @export
centrality_power <- function(...) {
  graph <- .G()
  power_centrality(graph = graph, nodes = V(graph), ...)
}
#' @describeIn centrality Wrapper for [igraph::closeness()] and [igraph::estimate_closeness()]
#' @importFrom igraph V closeness estimate_closeness
#' @importFrom rlang tidy_quotes
#' @export
centrality_closeness <- function(...) {
  graph <- .G()
  dots <- tidy_quotes(...)
  if (is.null(dots$cutoff)) {
    closeness(graph = graph, vids = V(graph), ...)
  } else {
    estimate_closeness(graph = graph, vids = V(graph), ...)
  }
}
#' @describeIn centrality Wrapper for [igraph::eigen_centrality()]
#' @importFrom igraph eigen_centrality
#' @export
centrality_eigen <- function(...) {
  eigen_centrality(graph = .G(), ...)$vector
}
#' @describeIn centrality Wrapper for [igraph::hub_score()]
#' @importFrom igraph hub_score
#' @export
centrality_hub <- function(...) {
  hub_score(graph = .G(), ...)$vector
}
#' @describeIn centrality Wrapper for [igraph::page_rank()]
#' @importFrom igraph V page_rank
#' @export
centrality_pagerank <- function(...) {
  graph <- .G()
  page_rank(graph = graph, vids = V(graph), ...)$vector
}
#' @describeIn centrality Wrapper for [igraph::subgraph_centrality()]
#' @importFrom igraph subgraph_centrality
#' @export
centrality_subgraph <- function(...) {
  subgraph_centrality(graph = .G(), ...)
}
#' @describeIn centrality Wrapper for [igraph::degree()] and [igraph::strength()]
#' @importFrom igraph V degree strength
#' @importFrom rlang tidy_quotes
#' @export
centrality_degree <- function(...) {
  graph <- .G()
  dots <- tidy_quotes(...)
  if (is.null(dots$weights)) {
    degree(graph = graph, v = V(graph), ...)
  } else {
    strength(graph = graph, vids = V(graph), ...)
  }
}
