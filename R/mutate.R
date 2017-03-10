#' @export
#' @importFrom dplyr mutate
mutate.tbl_graph <- function(.data, ...) {
  d_tmp <- as_tibble(.data)
  d_tmp <- mutate(d_tmp, ...)
  set_graph_data(.data, d_tmp)
}
#' @export
dplyr::mutate

#' @importFrom dplyr transmute
#' @export
dplyr::transmute

#' @importFrom dplyr mutate_all
#' @export
dplyr::mutate_all

#' @importFrom dplyr mutate_at
#' @export
dplyr::mutate_at
