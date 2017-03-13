#' @export
#' @importFrom dplyr rename
rename.tbl_graph <- function(.data, ...) {
  .graph_context$set(.data)
  on.exit(.graph_context$clear())
  d_tmp <- as_tibble(.data)
  d_tmp <- rename(d_tmp, ...)
  set_graph_data(.data, d_tmp)
}
#' @export
dplyr::rename
