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
#' @describeIn edge_types Query whether an edge goes from a set of nodes
#' @param from,to,nodes A vector giving node indices
#' @export
edge_is_from <- function(from) {
  expect_edges()
  .free_graph_context()
  .E()$from %in% as_ind(from, graph_order())
}
#' @describeIn edge_types Query whether an edge goes to a set of nodes
#' @export
edge_is_to <- function(to) {
  expect_edges()
  .free_graph_context()
  .E()$to %in% as_ind(to, graph_order())
}
#' @describeIn edge_types Query whether an edge goes between two sets of nodes
#' @param ignore_dir Is both directions of the edge allowed
#' @export
edge_is_between <- function(from, to, ignore_dir = !graph_is_directed()) {
  expect_edges()
  .free_graph_context()
  edges <- .E()
  from <- as_ind(from, graph_order())
  to <- as_ind(to, graph_order())
  include <- edges$from %in% from & edges$to %in% to
  if (ignore_dir) {
    include2 <- edges$to %in% from & edges$from %in% to
    include <- include | include2
  }
  include
}
#' @describeIn edge_types Query whether an edge goes from or to a set of nodes
#' @export
edge_is_incident <- function(nodes) {
  expect_edges()
  .free_graph_context()
  edges <- .E()
  nodes <- as_ind(nodes, graph_order())
  edges$from %in% nodes | edges$to %in% nodes
}
#' @describeIn edge_types Query whether an edge is a bridge (ie. it's removal
#' will increase the number of components in a graph)
#' @importFrom igraph bridges gsize
#' @export
edge_is_bridge <- function() {
  expect_edges()
  graph <- .G()
  seq_len(gsize(graph)) %in% bridges(graph)
}

