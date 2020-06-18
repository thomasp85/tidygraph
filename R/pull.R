#' @export
#' @importFrom dplyr pull
pull.tbl_graph <- function(.data, var = -1, name = NULL, ...) {
  d_tmp <- as_tibble(.data)
  var <- enquo(var)
  name <- enquo(name)
  pull(d_tmp, !! var, !! name, ...)
}
#' @export
#' @importFrom dplyr pull
pull.morphed_tbl_graph <- function(.data, var = -1, name = NULL, ...) {
  var <- enquo(var)
  name <- enquo(name)
  lapply(.data, pull, !! var, !! name, ...)
}
#' @export
dplyr::pull
