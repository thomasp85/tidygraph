#' Node properties related to the graph topology
#'
#' These functions calculate properties that are dependent on the overall
#' topology of the graph.
#'
#' @return A vector of the same length as the number of nodes in the graph
#'
#' @name node_topology
#' @rdname node_topology
#'
#' @examples
#' # Sort a graph based on its topological order
#' create_tree(10, 2) %>%
#'   arrange(sample(graph_order())) %>%
#'   mutate(old_ind = seq_len(graph_order())) %>%
#'   arrange(node_topo_order())
NULL

#' @describeIn node_topology Get the immediate dominator of each node. Wraps [igraph::dominator_tree()].
#' @importFrom igraph dominator_tree
#' @export
#'
#' @param root The node to start the dominator search from
#' @param mode How should edges be followed. Either `'in'` or `'out'`
node_dominator <- function(root, mode = 'out') {
  expect_nodes()
  graph <- .G()
  dom <- dominator_tree(graph, root, mode)$dom
  dom[is.nan(dom)] <- NA
  dom[root] <- NA
  dom
}
#' @describeIn node_topology Get the topological order of nodes in a DAG. Wraps [igraph::topo_sort()].
#' @importFrom igraph gorder topo_sort
#' @export
node_topo_order <- function(mode = 'out') {
  expect_nodes()
  graph <- .G()
  match(seq_len(gorder(graph)), topo_sort(graph, mode = mode))
}
