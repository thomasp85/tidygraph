#' @describeIn tbl_graph Method for edgelist, adjacency and incidence matrices
#' @export
as_tbl_graph.matrix <- function(x, directed = TRUE, ...) {
  graph <- switch(
    guess_matrix_type(x),
    edgelist = as_graph_edgelist(x, directed, ...),
    adjacency = as_graph_adj_matrix(x, directed, ...),
    incidence = as_graph_incidence(x, directed, ...),
    unknown = cli::cli_abort('Unknown matrix format')
  )
  as_tbl_graph(graph)
}

guess_matrix_type <- function(x) {
  if (nrow(x) == ncol(x) &&
      mode(x) %in% c('numeric', 'integer') &&
      ((is.null(rownames(x)) && is.null(colnames(x))) ||
       all(colnames(x) == rownames(x)))) {
    'adjacency'
  } else if (ncol(x) == 2) {
    'edgelist'
  } else if (mode(x) %in% c('numeric', 'integer')) {
    'incidence'
  } else {
    'uknown'
  }
}

#' @importFrom igraph graph_from_edgelist
as_graph_edgelist <- function(x, directed, ...) {
  graph_from_edgelist(x, directed, ...)
}
#' @importFrom igraph graph_from_adjacency_matrix
as_graph_adj_matrix <- function(x, directed, ...) {
  graph_from_adjacency_matrix(x, mode = if (directed) 'directed' else 'undirected',
                              weighted = TRUE, ...)
}
#' @importFrom igraph graph_from_incidence_matrix
as_graph_incidence <- function(x, directed, ...) {
  graph_from_incidence_matrix(x, directed, mode = 'out', weighted = TRUE, ...)
}
