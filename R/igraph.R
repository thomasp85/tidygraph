#' @describeIn tbl_graph Method for igraph object. Simply subclasses the object into a `tbl_graph`
#' @export
as_tbl_graph.igraph <- function(x, ...) {
  class(x) <- c('tbl_graph', 'igraph')
  attr(x, 'active') <- 'nodes'
  x
}

#' @importFrom igraph as.igraph
#' @export
as.igraph.tbl_graph <- function(x, ...) {
  class(x) <- 'igraph'
  attr(x, 'active') <- NULL
  x
}
#' @export
igraph::as.igraph
