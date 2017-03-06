#' @export
activate <- function(.data, what) {
  what <- deparse(substitute(what))
  activate_(.data, what)
}
#' @export
activate_ <- function(.data, what) {
  UseMethod('activate_')
}
#' @export
activate_.tbl_graph <- function(.data, what) {
  active(.data) <- what
  .data
}
