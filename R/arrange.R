#' @export
#' @importFrom dplyr arrange
arrange.tbl_graph <- function(.data, ...) {
  .register_graph_context(.data)
  d_tmp <- as_tibble(.data)
  if ('.tbl_graph_index' %in% names(d_tmp)) {
    cli::cli_abort('The attribute name {.field .tbl_graph_index} is reserved')
  }
  orig_ind <- seq_len(nrow(d_tmp))
  d_tmp$.tbl_graph_index <- orig_ind
  d_tmp <- arrange(d_tmp, ...)

  switch(
    active(.data),
    nodes = permute_nodes(.data, d_tmp$.tbl_graph_index),
    edges = permute_edges(.data, d_tmp$.tbl_graph_index)
  ) %gr_attr% .data
}
#' @export
#' @importFrom dplyr arrange
arrange.morphed_tbl_graph <- function(.data, ...) {
  .data[] <- lapply(.data, arrange, ...)
  .data
}
#' @export
dplyr::arrange

#' @importFrom igraph is.directed as_data_frame
permute_edges <- function(graph, order) {
  graph_mod <- as_data_frame(graph, what = 'both')
  graph_mod$edges <- graph_mod$edges[order, ]
  as_tbl_graph(graph_mod, directed = is.directed(graph))
}
#' @importFrom igraph permute
permute_nodes <- function(graph, order) {
  permute(graph, match(seq_along(order), order))
}
