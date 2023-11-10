#' Querying graph types
#'
#' This set of functions lets the user query different aspects of the graph
#' itself. They are all concerned with wether the graph implements certain
#' properties and will all return a logical scalar.
#'
#' @param graph The graph to compare structure to
#'
#' @param method The algorithm to use for comparison
#'
#' @param ... Arguments passed on to the comparison methods. See
#' [igraph::is_isomorphic_to()] and [igraph::is_subgraph_isomorphic_to()]
#'
#' @return A logical scalar
#'
#' @name graph_types
#' @rdname graph_types
#'
#' @examples
#' gr <- create_tree(50, 4)
#'
#' with_graph(gr, graph_is_tree())
#'
NULL

#' @describeIn graph_types Is the graph simple (no parallel edges)
#' @importFrom igraph is_simple
#' @export
graph_is_simple <- function() {
  is_simple(.G())
}
#' @describeIn graph_types Is the graph directed
#' @importFrom igraph is_directed
#' @export
graph_is_directed <- function() {
  is_directed(.G())
}
#' @describeIn graph_types Is the graph bipartite
#' @importFrom igraph is_bipartite
#' @export
graph_is_bipartite <- function() {
  is_bipartite(.G())
}
#' @describeIn graph_types Is the graph connected
#' @importFrom igraph is_connected
#' @export
graph_is_connected <- function() {
  is_connected(.G())
}
#' @describeIn graph_types Is the graph a tree
#' @export
graph_is_tree <- function() {
  is_tree(.G())
}
#' @describeIn graph_types Is the graph an ensemble of multiple trees
#' @export
graph_is_forest <- function() {
  is_forest(.G())
}
#' @describeIn graph_types Is the graph a directed acyclic graph
#' @importFrom igraph is_dag
#' @export
graph_is_dag <- function() {
  is_dag(.G())
}
#' @describeIn graph_types Is the graph chordal
#' @importFrom igraph is_chordal
#' @export
graph_is_chordal <- function() {
  is_chordal(.G())$chordal
}
#' @describeIn graph_types Is the graph fully connected
#' @export
graph_is_complete <- function() {
  graph_is_simple() && all(centrality_degree(mode = 'all', loops = FALSE) == graph_order() - 1)
}
#' @describeIn graph_types Is the graph isomorphic to another graph. See [igraph::is_isomorphic_to()]
#' @importFrom igraph is_isomorphic_to
#' @export
graph_is_isomorphic_to <- function(graph, method = 'auto', ...) {
  is_isomorphic_to(.G(), graph, method, ...)
}
#' @describeIn graph_types Is the graph an isomorphic subgraph to another graph. see [igraph::is_subgraph_isomorphic_to()]
#' @importFrom igraph is_subgraph_isomorphic_to
#' @export
graph_is_subgraph_isomorphic_to <- function(graph, method = 'auto', ...) {
  is_subgraph_isomorphic_to(.G(), graph, method, ...)
}
