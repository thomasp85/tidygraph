#' @export
#' @importFrom dplyr arrange
arrange.tbl_graph <- function(.data, ...) {
  .graph_context$set(.data)
  on.exit(.graph_context$clear())
  d_tmp <- as_tibble(.data)
  if ('.tbl_graph_index' %in% names(d_tmp)) {
    stop('The attribute name ".tbl_graph_index" is reserved', call. = FALSE)
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

#' @importFrom igraph graph_from_data_frame is.directed as_data_frame vertex_attr<-
permute_edges <- function(graph, order) {
  graph_mod <- as_data_frame(graph, what = 'both')
  graph_mod$edges <- graph_mod$edges[order, ]
  graph_new <- graph_from_data_frame(graph_mod$edges, is.directed(graph))
  vertex_attr(graph_new) <- as.list(graph_mod$vertices)
  graph_new
}
#' @importFrom igraph permute
permute_nodes <- function(graph, order) {
  permute(graph, match(seq_along(order), order))
}
