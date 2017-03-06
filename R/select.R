#' @export
#' @importFrom dplyr select_
select_.tbl_graph <- function(.data, ..., .dots) {
  d_tmp <- as_tibble(.data)
  d_tmp <- select_(d_tmp, ..., .dots = .dots)
  set_graph_data(.data, d_tmp)
}
