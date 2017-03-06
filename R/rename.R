#' @export
#' @importFrom dplyr rename_
rename_.tbl_graph <- function(.data, ..., .dots) {
  d_tmp <- as_tibble(.data)
  d_tmp <- rename_(d_tmp, ..., .dots = .dots)
  set_graph_data(.data, d_tmp)
}
