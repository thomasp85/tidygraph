#' @export
as_tbl_graph <- function(x) {
  UseMethod('as_tbl_graph')
}
#' @export
as_tbl_graph.igraph <- function(x) {
  class(x) <- c('tbl_graph', 'igraph')
  attr(x, 'active') <- 'nodes'
  x
}
active <- function(x) {
+0+  attr(x, 'active')
}
`active<-` <- function(x, value) {
  attr(x, 'active') <- value
  x
}
