#' @export
as_tbl_graph <- function(x, ...) {
  UseMethod('as_tbl_graph')
}
#' @export
as_tbl_graph.igraph <- function(x, ...) {
  class(x) <- c('tbl_graph', 'igraph')
  attr(x, 'active') <- 'nodes'
  x
}
#' @export
#' @importFrom igraph as.igraph
as_tbl_graph.default <- function(x, ...) {
  tryCatch({
    as_tbl_graph(as.igraph(x))
  }, error = function(e) stop('No support for ', class(x)[1], ' objects', call. = FALSE))
}
#' @importFrom dplyr tbl_vars
#' @export
tbl_vars.tbl_graph <- function(x) {
  names(as_tibble(x))
}
#' @importFrom dplyr groups
#' @export
groups.tbl_graph <- function(x) {
  NULL
}
