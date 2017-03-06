#' @export
#' @importFrom tibble as_tibble
as_tibble.tbl_graph <- function(x, active = NULL, ...) {
  if (is.null(active)) {
    active <- attr(x, 'active')
  }
  switch(
    active,
    nodes = node_tibble(x),
    edges = edge_tibble(x),
    stop('Unknown active element: ', active, '. Only nodes and edges supported', call. = FALSE)
  )
}
#' @importFrom igraph vertex_attr gorder
#' @importFrom tibble as_tibble
node_tibble <- function(x) {
  tbl <- as_tibble(vertex_attr(x))
  if (length(attr(tbl, 'row.names')) == 0) {
    attr(tbl, 'row.names') <- seq_len(gorder(x))
  }
  tbl
}
#' @importFrom igraph edge_attr gsize as_edgelist
#' @importFrom tibble as_tibble
#' @importFrom dplyr bind_cols
edge_tibble <- function(x) {
  tbl <- as_tibble(edge_attr(x))
  if (length(attr(tbl, 'row.names')) == 0) {
    attr(tbl, 'row.names') <- seq_len(gsize(x))
  }
  e_list <- as_tibble(as_edgelist(x, names = FALSE))
  names(e_list) <- c('from', 'to')
  bind_cols(e_list, tbl)
}
set_graph_data <- function(x, value) {
  switch(
    active(x),
    nodes = set_node_attributes(x, value),
    edges = set_edge_attributes(x, value),
    stop('Unknown active element: ', active(x), '. Only nodes and edges supported', call. = FALSE)
  )
}
#' @importFrom igraph vertex_attr<-
set_node_attributes <- function(x, value) {
  vertex_attr(x) <- as.list(value)
  x
}
#' @importFrom igraph edge_attr<-
set_edge_attributes <- function(x, value) {
  value <- value[, !names(value) %in% c('from', 'to')]
  edge_attr(x) <- as.list(value)
  x
}
