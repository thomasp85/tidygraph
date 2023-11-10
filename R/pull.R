# TODO: Update signature to include `name` once we begin to depend on dplyr 1.0

#' @export
#' @importFrom dplyr pull
pull.tbl_graph <- function(.data, var = -1, ...) {
  d_tmp <- as_tibble(.data)
  var <- enquo(var)
  pull(d_tmp, !! var, ...)
}
#' @export
#' @importFrom dplyr pull
pull.morphed_tbl_graph <- function(.data, var = -1, ...) {
  var <- enquo(var)
  lapply(.data, pull, !! var, ...)
}
#' @export
dplyr::pull
