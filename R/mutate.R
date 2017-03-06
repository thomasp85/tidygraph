#' @export
#' @importFrom dplyr mutate_
mutate_.tbl_graph <- function(.data, ..., .dots) {
  d_tmp <- as_tibble(.data)
  d_tmp <- mutate_(d_tmp, ..., .dots = .dots)
  set_graph_data(.data, d_tmp)
}
