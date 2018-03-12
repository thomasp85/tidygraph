#' @export
#' @importFrom dplyr select
select.tbl_graph <- function(.data, ...) {
  .register_graph_context(.data)
  d_tmp <- as_tibble(.data)
  d_tmp <- select(d_tmp, ...)
  set_graph_data(.data, d_tmp)
}
#' @export
#' @importFrom dplyr select
select.morphed_tbl_graph <- function(.data, ...) {
  .data[] <- lapply(.data, protect_ind, .f = select, ...)
  .data
}
#' @export
dplyr::select

#' @importFrom dplyr contains
#' @export
dplyr::contains

#' @importFrom dplyr ends_with
#' @export
dplyr::ends_with

#' @importFrom dplyr everything
#' @export
dplyr::everything

#' @importFrom dplyr matches
#' @export
dplyr::matches

#' @importFrom dplyr num_range
#' @export
dplyr::num_range

#' @importFrom dplyr one_of
#' @export
dplyr::one_of

#' @importFrom dplyr starts_with
#' @export
dplyr::starts_with
