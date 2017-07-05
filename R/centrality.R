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
#' @param weights The weight of the edges to use for the calculation. Will be
#' evaluated in the context of the edge data.
#' @param mode How should edges be followed. Ignored for undirected graphs
#' @param directed Should direction of edges be used for the calculations
#' @param loops Should loops be included in the calculation
#' @param scale Should the output be scaled between 0 and 1
#' @param rescale Should the output be scaled to sum up to 1
#' @param normalized Should the output be normalized
#' @param tol Tolerance for near-singularities during matrix inversion
#' @param options Settings passed on to `igraph::arpack()`
#' @param cutoff maximum path length to use during calculations
#' @param nobigint Should big integers be avoided during calculations
#' @param alpha Relative importance of endogenous vs exogenous factors
#' @param exo The exogenous factors of the nodes. Either a scalar or a number
#' number for each node. Evaluated in the context of the node data.
#' @param exponent The decay rate for the Bonacich power centrality
#' @param damping The damping factor of the page rank algorithm
#' @param personalized The probability of jumping to a node when abandoning a
#' random walk. Evaluated in the context of the node data.
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
centrality_alpha <- function(weights = NULL, alpha = 1, exo = 1, tol = 1e-7, loops = FALSE) {
  expect_nodes()
  graph <- .G()
  weights <- enquo(weights)
  weights <- eval_tidy(weights, .E())
  exo <- enquo(exo)
  exo <- eval_tidy(exo, .N())
  alpha_centrality(graph = graph, nodes = V(graph), alpha = alpha, exo = exo, weights = weights, tol = tol, loops = loops)
}
#' @describeIn centrality Wrapper for [igraph::authority_score()]
#' @importFrom igraph authority_score
#' @export
centrality_authority <- function(weights = NULL, scale = TRUE, options = igraph::arpack_defaults) {
  expect_nodes()
  weights <- enquo(weights)
  weights <- eval_tidy(weights, .E())
  authority_score(graph = .G(), scale = scale, weights = weights, options = options)$vector
}
#' @describeIn centrality Wrapper for [igraph::betweenness()] and [igraph::estimate_betweenness()]
#' @importFrom igraph V betweenness estimate_betweenness
#' @importFrom rlang quos
#' @export
centrality_betweenness <- function(weights = NULL, directed = TRUE, cutoff = NULL, nobigint = TRUE, normalized = FALSE) {
  expect_nodes()
  graph <- .G()
  weights <- enquo(weights)
  weights <- eval_tidy(weights, .E())
  if (is.null(cutoff)) {
    betweenness(graph = graph, v = V(graph), directed = directed, weights = weights, nobigint = nobigint, normalized = normalized)
  } else {
    estimate_betweenness(graph = graph, vids = V(graph), directed = directed, cutoff = cutoff, weights = weights, nobigint = nobigint)
  }
}
#' @describeIn centrality Wrapper for [igraph::power_centrality()]
#' @importFrom igraph V power_centrality
#' @export
centrality_power <- function(exponent = 1, rescale = FALSE, tol = 1e-7, loops = FALSE) {
  expect_nodes()
  graph <- .G()
  power_centrality(graph = graph, nodes = V(graph), exponent = exponent, loops = loops, rescale = rescale, tol = tol)
}
#' @describeIn centrality Wrapper for [igraph::closeness()] and [igraph::estimate_closeness()]
#' @importFrom igraph V closeness estimate_closeness
#' @importFrom rlang quos
#' @export
centrality_closeness <- function(weights = NULL, mode = 'out', normalized = FALSE, cutoff = NULL) {
  expect_nodes()
  graph <- .G()
  weights <- enquo(weights)
  weights <- eval_tidy(weights, .E())
  if (is.null(cutoff)) {
    closeness(graph = graph, vids = V(graph), mode = mode, weights = weights, normalized = normalized)
  } else {
    estimate_closeness(graph = graph, vids = V(graph), mode = mode, cutoff = cutoff, weights = weights, normalized = normalized)
  }
}
#' @describeIn centrality Wrapper for [igraph::eigen_centrality()]
#' @importFrom igraph eigen_centrality
#' @export
centrality_eigen <- function(weights = NULL, directed = FALSE, scale = TRUE, options = igraph::arpack_defaults) {
  expect_nodes()
  weights <- enquo(weights)
  weights <- eval_tidy(weights, .E())
  eigen_centrality(graph = .G(), directed = directed, scale = scale, weights = weights, options = options)$vector
}
#' @describeIn centrality Wrapper for [igraph::hub_score()]
#' @importFrom igraph hub_score
#' @export
centrality_hub <- function(weights = NULL, scale = TRUE, options = igraph::arpack_defaults) {
  expect_nodes()
  weights <- enquo(weights)
  weights <- eval_tidy(weights, .E())
  hub_score(graph = .G(), scale = scale, weights = weights, options = options)$vector
}
#' @describeIn centrality Wrapper for [igraph::page_rank()]
#' @importFrom igraph V page_rank
#' @export
centrality_pagerank <- function(weights = NULL, directed = TRUE, damping = 0.85, personalized = NULL) {
  expect_nodes()
  graph <- .G()
  weights <- enquo(weights)
  weights <- eval_tidy(weights, .E())
  personalized <- enquo(personalized)
  personalized <- eval_tidy(personalized, .N())
  page_rank(graph = graph, vids = V(graph), directed = directed, damping = damping, personalized = personalized, weights = weights)$vector
}
#' @describeIn centrality Wrapper for [igraph::subgraph_centrality()]
#' @importFrom igraph subgraph_centrality
#' @export
centrality_subgraph <- function(loops = FALSE) {
  expect_nodes()
  subgraph_centrality(graph = .G(), diag = loops)
}
#' @describeIn centrality Wrapper for [igraph::degree()] and [igraph::strength()]
#' @importFrom igraph V degree strength
#' @importFrom rlang quos
#' @export
centrality_degree <- function(weights = NULL, mode = 'out', loops = TRUE, normalized = FALSE) {
  expect_nodes()
  graph <- .G()
  weights <- enquo(weights)
  weights <- eval_tidy(weights, .E())
  if (is.null(weights)) {
    degree(graph = graph, v = V(graph), mode = mode, loops = loops, normalized = normalized)
  } else {
    strength(graph = graph, vids = V(graph), mode = mode, loops = loops, weights = weights)
  }
}
#' @describeIn centrality Wrapper for [igraph::edge_betweenness()]
#' @importFrom igraph edge_betweenness estimate_edge_betweenness E
#' @importFrom rlang quos
#' @export
centrality_edge_betweenness <- function(weights = NULL, directed = TRUE, cutoff = NULL) {
  expect_edges()
  graph <- .G()
  weights <- enquo(weights)
  weights <- eval_tidy(weights, .E())
  if (is.null(cutoff)) {
    edge_betweenness(graph = graph, e = E(graph), directed = directed, weights = weights)
  } else {
    estimate_edge_betweenness(graph = graph, e = E(graph), directed = directed, cutoff = cutoff, weights = weights)
  }
}
