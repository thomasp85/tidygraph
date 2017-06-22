#' @export
#' @importFrom dplyr mutate
mutate.tbl_graph <- function(.data, ...) {
  .graph_context$set(.data)
  on.exit(.graph_context$clear())
  d_tmp <- as_tibble(.data)
  d_tmp <- mutate(d_tmp, ...)
  set_graph_data(.data, d_tmp)
}
#' @export
#' @importFrom dplyr mutate
mutate.morphed_tbl_graph <- function(.data, ...) {
  .data[] <- lapply(.data, protect_ind, .f = mutate, ...)
  .data
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

#' @importFrom dplyr n
#' @export
dplyr::n
