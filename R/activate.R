#' @export
activate <- function(.data, what) {
  UseMethod('activate')
}
#' @export
#' @importFrom rlang tidy_eval tidy_quote as_quosure
activate.tbl_graph <- function(.data, what) {
  what_name <- deparse(substitute(what))
  if (what_name %in% c('nodes', 'edges')) what <- what_name
  active(.data) <- what
  .data
}
#' @export
activate.grouped_tbl_graph <- function(.data, what) {
  message('Ungrouping graph...')
  activate(ungroup(.data), what)
}

#' @export
active <- function(x) {
  attr(x, 'active')
}
`active<-` <- function(x, value) {
  if (!value %in% c('nodes', 'edges')) {
    stop('Only possible to activate nodes and edges', call. = FALSE)
  }
  attr(x, 'active') <- value
  x
}
