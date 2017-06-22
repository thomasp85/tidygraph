#' @importFrom igraph graph_attr graph_attr<-
`%gr_attr%` <- function(e1, e2) {
  graph_attr(e1) <- graph_attr(e2)
  attributes(e1) <- attributes(e2)
  e1
}

as_ind <- function(i, length) {
  seq_len(length)[i]
}
