#' @describeIn tbl_graph Method for edge table and set membership table
#' @export
#' @importFrom igraph graph_from_data_frame
as_tbl_graph.data.frame <- function(x, directed = TRUE, ...) {
  graph <- switch(
    guess_df_type(x),
    edge_df = as_graph_edge_df(x, directed),
    set_df = as_graph_set_df(x)
  )

  as_tbl_graph(graph)
}
guess_df_type <- function(x) {
  if (all(c('to', 'from') %in% names(x))) return('edge_df')
  if (all(vapply(x, inherits, logical(1), 'logical'))) return('set_df')
  if (all(vapply(x, function(col) all(unique(col) %in% c(0,1)), logical(1)))) return('set_df')
  'edge_df'
}
as_graph_edge_df <- function(x, directed) {
  from_ind <- which(names(x) == 'from')
  if (length(from_ind) == 0) from_ind <- 1
  to_ind <- which(names(x) == 'to')
  if (length(to_ind) == 0) to_ind <- 2
  x <- x[, c(from_ind, to_ind, seq_along(x)[-c(from_ind, to_ind)]), drop = FALSE]
  graph_from_data_frame(x, directed = directed)
}
as_graph_set_df <- function(x, simple = TRUE) {
  if (simple) {
    x <- as.matrix(x)
    mode(x) <- 'integer'
    adj_mat <- x %*% t(x)
    if (!is.null(attr(x, 'row.names'))) {
      colnames(adj_mat) <- rownames(adj_mat) <- row.names(x)
    }
    as_graph_adj_matrix(adj_mat, FALSE)
  } else {
    edges <- do.call(rbind, lapply(names(x), function(name) {
      nodes <- which(as.logical(x[[name]]))
      edges <- expand.grid(nodes, nodes)
      names(edges) <- c('from', 'to')
      edges$type <- name
      edges[edges$from != edges$to, , drop = FALSE]
    }))
    if (!is.null(attr(x, 'row.names'))) {
      nodes <- data.frame(name = row.names(x), stringsAsFactors = FALSE)
    } else {
      nodes <- as.data.frame(matrix(ncol = 0, nrow = nrow(x)))
    }
    as_graph_node_edge(list(nodes = nodes, edges = edges), FALSE)
  }
}
#' @export
as.data.frame.tbl_graph <- function(x, row.names = NULL, optional = FALSE, active = NULL, ...) {
  as.data.frame(as_tibble(x, active = active))
}
