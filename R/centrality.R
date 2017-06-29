#' Calculate node and edge centrality
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
#' create_notable('bull') %>%
#'   activate(nodes) %>%
#'   mutate(importance = centrality_alpha())
#'
#' # Most centrality measures are for nodes but not all
#' create_notable('bull') %>%
#'   activate(edges) %>%
#'   mutate(importance = centrality_edge_betweenness())
NULL

#' @describeIn centrality Wrapper for [igraph::alpha_centrality()]
#' @importFrom igraph V alpha_centrality
#' @export
centrality_alpha <- function(...) {
  expect_nodes()
  graph <- .G()
  alpha_centrality(graph = graph, nodes = V(graph), ...)
}
#' @describeIn centrality Wrapper for [igraph::authority_score()]
#' @importFrom igraph authority_score
#' @export
centrality_authority <- function(...) {
  expect_nodes()
  authority_score(graph = .G(), ...)$vector
}
#' @describeIn centrality Wrapper for [igraph::betweenness()] and [igraph::estimate_betweenness()]
#' @importFrom igraph V betweenness estimate_betweenness
#' @importFrom rlang quos
#' @export
centrality_betweenness <- function(...) {
  expect_nodes()
  graph <- .G()
  dots <- quos(...)
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
  expect_nodes()
  graph <- .G()
  power_centrality(graph = graph, nodes = V(graph), ...)
}
#' @describeIn centrality Wrapper for [igraph::closeness()] and [igraph::estimate_closeness()]
#' @importFrom igraph V closeness estimate_closeness
#' @importFrom rlang quos
#' @export
centrality_closeness <- function(...) {
  expect_nodes()
  graph <- .G()
  dots <- quos(...)
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
  expect_nodes()
  eigen_centrality(graph = .G(), ...)$vector
}
#' @describeIn centrality Wrapper for [igraph::hub_score()]
#' @importFrom igraph hub_score
#' @export
centrality_hub <- function(...) {
  expect_nodes()
  hub_score(graph = .G(), ...)$vector
}
#' @describeIn centrality Wrapper for [igraph::page_rank()]
#' @importFrom igraph V page_rank
#' @export
centrality_pagerank <- function(...) {
  expect_nodes()
  graph <- .G()
  page_rank(graph = graph, vids = V(graph), ...)$vector
}
#' @describeIn centrality Wrapper for [igraph::subgraph_centrality()]
#' @importFrom igraph subgraph_centrality
#' @export
centrality_subgraph <- function(...) {
  expect_nodes()
  subgraph_centrality(graph = .G(), ...)
}
#' @describeIn centrality Wrapper for [igraph::degree()] and [igraph::strength()]
#' @importFrom igraph V degree strength
#' @importFrom rlang quos
#' @export
centrality_degree <- function(...) {
  expect_nodes()
  graph <- .G()
  dots <- quos(...)
  if (is.null(dots$weights)) {
    degree(graph = graph, v = V(graph), ...)
  } else {
    strength(graph = graph, vids = V(graph), ...)
  }
}
#' @describeIn centrality Wrapper for [igraph::edge_betweenness()]
#' @importFrom igraph edge_betweenness estimate_edge_betweenness E
#' @importFrom rlang quos
#' @export
centrality_edge_betweenness <- function(...) {
  expect_edges()
  graph <- .G()
  dots <- quos(...)
  if (is.null(dots$cutoff)) {
    edge_betweenness(graph = graph, e = E(graph), ...)
  } else {
    estimate_edge_betweenness(graph = graph, e = E(graph), ...)
  }
}
