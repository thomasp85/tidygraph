#' Querying edge types
#'
#' These functions lets the user query whether the edges in a graph is of a
#' specific type. All functions return a logical vector giving whether each edge
#' in the graph corresponds to the specific type.
#'
#' @return A logical vector of the same length as the number of edges in the
#' graph
#'
#' @name edge_types
#' @rdname edge_types
#'
#' @examples
#' create_star(10, directed = TRUE, mutual = TRUE) %>%
#'   activate(edges) %>%
#'   sample_frac(0.7) %>%
#'   mutate(single_edge = !edge_is_mutual())
NULL

#' @describeIn edge_types Query whether each edge has any parallel siblings
#' @importFrom igraph which_multiple
#' @export
edge_is_multiple <- function() {
  expect_edges()
  graph <- .G()
  which_multiple(graph)
}
#' @describeIn edge_types Query whether each edge is a loop
#' @importFrom igraph which_loop
#' @export
edge_is_loop <- function() {
  expect_edges()
  graph <- .G()
  which_loop(graph)
}
#' @describeIn edge_types Query whether each edge has a sibling going in the reverse direction
#' @importFrom igraph which_mutual
#' @export
edge_is_mutual <- function() {
  expect_edges()
  graph <- .G()
  which_mutual(graph)
}
