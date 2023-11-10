#' @importFrom tidyr replace_na
#' @export
#'
replace_na.tbl_graph <- function(data, replace, ...) {
  d_tmp <- as_tibble(data)
  d_tmp <- replace_na(d_tmp, replace = replace, ...)
  set_graph_data(data, d_tmp)
}
#' @importFrom tidyr replace_na
#' @export
#'
replace_na.morphed_tbl_graph <- function(data, replace, ...) {
  .data[] <- lapply(data, replace_na, replace = replace, ...)
  .data
}
#' @export
tidyr::replace_na

#' @importFrom tidyr drop_na
#' @export
#'
drop_na.tbl_graph <- function(data, ...) {
  graph_slicer(data, drop_na, ...)
}
#' @importFrom tidyr drop_na
#' @export
#'
drop_na.morphed_tbl_graph <- function(data, ...) {
  .data[] <- lapply(.data, drop_na, ...)
  .data
}
#' @export
tidyr::drop_na
