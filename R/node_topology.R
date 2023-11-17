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
  root <- as_node_ind(root, graph)
  domtree <- as_edgelist(dominator_tree(graph, root, mode)$domtree)
  dom <- rep(NA, gorder(graph))
  dom[domtree[, 2]] <- domtree[, 1]
  dom[focus_ind(graph, 'nodes')]
}
#' @describeIn node_topology Get the topological order of nodes in a DAG. Wraps [igraph::topo_sort()].
#' @importFrom igraph gorder topo_sort
#' @export
node_topo_order <- function(mode = 'out') {
  expect_nodes()
  graph <- .G()
  compress_rank(match(focus_ind(graph, 'nodes'), topo_sort(graph, mode = mode)))
}
