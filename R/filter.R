#' @export
#' @importFrom dplyr filter_
#' @importFrom igraph delete_vertices delete_edges
filter_.tbl_graph <- function(.data, ..., .dots) {
  d_tmp <- as_tibble(.data)
  if ('.tbl_graph_index' %in% names(d_tmp)) {
    stop('The attribute name ".tbl_graph_index" is reserved', call. = FALSE)
  }
  orig_ind <- seq_len(nrow(d_tmp))
  d_tmp$.tbl_graph_index <- orig_ind
  d_tmp <- filter_(d_tmp, ..., .dots = .dots)
  remove_ind <- orig_ind[-d_tmp$.tbl_graph_index]
  switch(
    active(.data),
    nodes = delete_vertices(.data, remove_ind),
    edges = delete_edges(.data, remove_ind)
  ) %gr_attr% .data
}
