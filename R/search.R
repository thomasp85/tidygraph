#' Search a graph with depth first and breath first
#'
#' These functions wraps the [igraph::bfs()] and [igraph::dfs()] functions to
#' provide a consistent return value that can be used in [dplyr::mutate()]
#' calls. Each function returns an integer vector with values matching the order
#' of the nodes in the graph.
#'
#' @param root The node to start the search from
#'
#' @param mode How edges are followed in the search if the graph is directed.
#' `"out"` only follows outbound edges, `"in"` only follows inbound edges, and
#' `"all"` or `"total"` follows all edges. This is ignored for undirected
#' graphs.
#'
#' @param unreachable Should the search jump to a new component if the search is
#' terminated without all nodes being visited? Default to `FALSE` (only reach
#' connected nodes).
#'
#' @return An integer vector, the nature of which is determined by the function.
#'
#' @name search_graph
#' @rdname search_graph
#'
#' @examples
#' # Get the depth of each node in a tree
#' create_tree(10, 2) %>%
#'   activate(nodes) %>%
#'   mutate(depth = bfs_dist(root = 1))
#'
#' # Reorder nodes based on a depth first search from node 3
#' create_notable('franklin') %>%
#'   activate(nodes) %>%
#'   mutate(order = dfs_rank(root = 3)) %>%
#'   arrange(order)
#'
NULL

# Breath First Search -----------------------------------------------------

#' @describeIn search_graph Get the succession in which the nodes are visited in a breath first search
#' @importFrom igraph bfs gorder
#' @export
bfs_rank <- function(root, mode = 'out', unreachable = FALSE) {
  expect_nodes()
  graph <- .G()
  root <- as_ind(root, gorder(graph))
  ind <- bfs(graph = graph, root = root, neimode = mode, unreachable = unreachable,
             order = TRUE, rank = TRUE)$rank
  as.integer(ind)
}
#' @describeIn search_graph Get the nodes from which each node is visited in a breath first search
#' @importFrom igraph bfs gorder
#' @export
bfs_parent <- function(root, mode = 'out', unreachable = FALSE) {
  expect_nodes()
  graph <- .G()
  root <- as_ind(root, gorder(graph))
  ind <- bfs(graph = graph, root = root, neimode = mode, unreachable = unreachable,
      order = TRUE, father = TRUE)$father
  as.integer(ind)
}
#' @describeIn search_graph Get the node that was visited before each node in a breath first search
#' @importFrom igraph bfs gorder
#' @export
bfs_before <- function(root, mode = 'out', unreachable = FALSE) {
  expect_nodes()
  graph <- .G()
  root <- as_ind(root, gorder(graph))
  ind <- bfs(graph = graph, root = root, neimode = mode, unreachable = unreachable,
             order = TRUE, pred = TRUE)$pred
  as.integer(ind)
}
#' @describeIn search_graph Get the node that was visited after each node in a breath first search
#' @importFrom igraph bfs gorder
#' @export
bfs_after <- function(root, mode = 'out', unreachable = FALSE) {
  expect_nodes()
  graph <- .G()
  root <- as_ind(root, gorder(graph))
  ind <- bfs(graph = graph, root = root, neimode = mode, unreachable = unreachable,
             order = TRUE, succ = TRUE)$succ
  as.integer(ind)
}
#' @describeIn search_graph Get the number of nodes between the root and each node in a breath first search
#' @importFrom igraph bfs gorder
#' @export
bfs_dist <- function(root, mode = 'out', unreachable = FALSE) {
  expect_nodes()
  graph <- .G()
  root <- as_ind(root, gorder(graph))
  ind <- bfs(graph = graph, root = root, neimode = mode, unreachable = unreachable,
             order = TRUE, dist = TRUE)$dist
  as.integer(ind)
}

# Depth First Search ------------------------------------------------------

#' @describeIn search_graph Get the succession in which the nodes are visited in a depth first search
#' @importFrom igraph dfs gorder
#' @export
dfs_rank <- function(root, mode = 'out', unreachable = FALSE) {
  expect_nodes()
  graph <- .G()
  root <- as_ind(root, gorder(graph))
  ind <- dfs(graph = graph, root = root, neimode = mode, unreachable = unreachable,
             order = TRUE)$order
  match(seq_along(ind), as.integer(ind))
}
#' @describeIn search_graph Get the succession in which each nodes subtree is completed in a depth first search
#' @importFrom igraph dfs gorder
#' @export
dfs_rank_out <- function(root, mode = 'out', unreachable = FALSE) {
  expect_nodes()
  graph <- .G()
  root <- as_ind(root, gorder(graph))
  ind <- dfs(graph = graph, root = root, neimode = mode, unreachable = unreachable,
             order = TRUE, order.out = TRUE)$order.out
  match(seq_along(ind), as.integer(ind))
}
#' @describeIn search_graph Get the nodes from which each node is visited in a depth first search
#' @importFrom igraph dfs gorder
#' @export
dfs_parent <- function(root, mode = 'out', unreachable = FALSE) {
  expect_nodes()
  graph <- .G()
  root <- as_ind(root, gorder(graph))
  ind <- dfs(graph = graph, root = root, neimode = mode, unreachable = unreachable,
             order = TRUE, father = TRUE)$father
  as.integer(ind)
}
#' @describeIn search_graph Get the number of nodes between the root and each node in a depth first search
#' @importFrom igraph dfs gorder
#' @export
dfs_dist <- function(root, mode = 'out', unreachable = FALSE) {
  expect_nodes()
  graph <- .G()
  root <- as_ind(root, gorder(graph))
  ind <- dfs(graph = graph, root = root, neimode = mode, unreachable = unreachable,
             order = TRUE, dist = TRUE)$dist
  as.integer(ind)
}
