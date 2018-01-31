#' @export
#' @importFrom dplyr slice
#' @importFrom igraph delete_vertices delete_edges
slice.tbl_graph <- function(.data, ...) {
  .register_graph_context(.data)
  d_tmp <- as_tibble(.data)
  if ('.tbl_graph_index' %in% names(d_tmp)) {
    stop('The attribute name ".tbl_graph_index" is reserved', call. = FALSE)
  }
  orig_ind <- seq_len(nrow(d_tmp))
  d_tmp$.tbl_graph_index <- orig_ind
  d_tmp <- slice(d_tmp, ...)
  remove_ind <- if (nrow(d_tmp) == 0) orig_ind else orig_ind[-d_tmp$.tbl_graph_index]
  switch(
    active(.data),
    nodes = delete_vertices(.data, remove_ind),
    edges = delete_edges(.data, remove_ind)
  ) %gr_attr% .data
}
#' @export
#' @importFrom dplyr slice
slice.morphed_tbl_graph <- function(.data, ...) {
  .data[] <- lapply(.data, slice, ...)
  .data
}
#' @export
dplyr::slice
