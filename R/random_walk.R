#' Perform a random walk on the graph and return encounter rank
#'
#' A random walk is a traversal of the graph starting from a node and going a
#' number of steps by picking an edge at random (potentially weighted).
#' `random_walk()` can be called both when nodes and edges are active and will
#' adapt to return a value fitting to the currently active part. As the
#' walk order cannot be directly encoded in the graph the return value is a list
#' giving a vector of positions along the walk of each node or edge.
#'
#' @param n The number of steps to perform. If the walk gets stuck before
#' reaching this number the walk is terminated
#' @param root The node to start the walk at. If `NULL` a random node will be
#' used
#' @param mode How edges are followed in the search if the graph is directed.
#' `"out"` only follows outbound edges, `"in"` only follows inbound edges, and
#' `"all"` or `"total"` follows all edges. This is ignored for undirected
#' graphs.
#' @param weights The weights to use for edges when selecting the next step of
#' the walk. Currently only used when edges are active
#'
#' @return A list with an integer vector for each node or edge (depending on
#' what is active) each element encode the time the node/edge is encountered
#' along the walk
#'
#' @importFrom igraph random_walk random_edge_walk gorder gsize
#' @importFrom rlang enquo quo_is_null eval_tidy
#' @export
#'
#' @examples
#' graph <- create_notable("zachary")
#'
#' # Random walk returning node order
#' graph |>
#'   mutate(walk_rank = random_walk_rank(200))
#'
#' # Rank edges instead
#' graph |>
#'   activate(edges) |>
#'   mutate(walk_rank = random_walk_rank(200))
#'
random_walk_rank <- function(n, root = NULL, mode = "out", weights = NULL) {
  graph <- .G()
  if (is.null(root)) {
    root <- sample(gorder(graph), 1)
  } else {
    root <- as_node_ind(root, graph)
  }
  weights <- enquo(weights)
  if (active(graph) == "nodes") {
    if (!quo_is_null(weights)) {
      cli::cli_warn('{.arg weights} is ignored when doing a random walk on nodes')
    }
    walk <- as.integer(random_walk(graph, root, n, mode = mode))
    len_out <- gorder(graph)
  } else {
    weights <- eval_tidy(weights, .E(focused = FALSE)) %||% NA
    walk <- as.integer(random_edge_walk(graph, root, n, weights, mode = mode))
    len_out <- gsize(graph)
  }
  res <- rep(list(integer()), len_out)
  ord <- split(seq_along(walk), walk)
  res[as.integer(names(ord))] <- ord
  res[focus_ind(graph, active(graph))]
}
