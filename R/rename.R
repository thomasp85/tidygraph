#' @export
#' @importFrom dplyr rename
rename.tbl_graph <- function(.data, ...) {
  d_tmp <- as_tibble(.data)
  d_tmp <- rename(d_tmp, ...)
  set_graph_data(.data, d_tmp)
}
#' @export
dplyr::rename
