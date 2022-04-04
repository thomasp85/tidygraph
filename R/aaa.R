#' @importFrom igraph graph_attr graph_attr<-
`%gr_attr%` <- function(e1, e2) {
  graph_attr(e1) <- graph_attr(e2)
  attributes(e1) <- attributes(e2)
  e1
}

as_ind <- function(i, length) {
  seq_len(length)[i]
}

expect_influencer <- function(...) {
  rlang::check_installed('influenceR', ...)
}
expect_netrankr <- function(...) {
  rlang::check_installed('netrankr', ...)
}
expect_seriation <- function(...) {
  rlang::check_installed('seriation', ...)
}
expect_netswan <- function(...) {
  rlang::check_installed('NetSwan', ...)
}
