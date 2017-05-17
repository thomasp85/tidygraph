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
NULL

#' @describeIn edge_types Query whether each edge has any parallel siblings
#' @importFrom igraph gsize which_multiple
#' @export
edge_multiple <- function() {
  expect_edges()
  graph <- .G()
  seq_len(gsize(graph)) %in% which_multiple(graph)
}
#' @describeIn edge_types Query whether each edge is a loop
#' @importFrom igraph gsize which_loop
#' @export
edge_loop <- function() {
  expect_edges()
  graph <- .G()
  seq_len(gsize(graph)) %in% which_loop(graph)
}
#' @describeIn edge_types Query whether each edge has a sibling going in the reverse direction
#' @importFrom igraph gsize which_mutual
#' @export
edge_mutual <- function() {
  expect_edges()
  graph <- .G()
  seq_len(gsize(graph)) %in% which_mutual(graph)
}
