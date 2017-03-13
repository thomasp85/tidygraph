#' @importFrom igraph V alpha_centrality
#' @export
centrality_alpha <- function(...) {
  graph <- .G()
  alpha_centrality(graph = graph, nodes = V(graph), ...)
}
#' @importFrom igraph authority_score
#' @export
centrality_authority <- function(...) {
  authority_score(graph = .G(), ...)$vector
}
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
#' @importFrom igraph V power_centrality
#' @export
centrality_power <- function(...) {
  graph <- .G()
  power_centrality(graph = graph, nodes = V(graph), ...)
}
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
#' @importFrom igraph eigen_centrality
#' @export
centrality_eigen <- function(...) {
  eigen_centrality(graph = .G(), ...)$vector
}
#' @importFrom igraph hub_score
#' @export
centrality_hub <- function(...) {
  hub_score(graph = .G(), ...)$vector
}
#' @importFrom igraph V page_rank
#' @export
centrality_pagerank <- function(...) {
  graph <- .G()
  page_rank(graph = graph, vids = V(graph), ...)$vector
}
#' @importFrom igraph subgraph_centrality
#' @export
centrality_subgraph <- function(...) {
  subgraph_centrality(graph = .G(), ...)
}
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
