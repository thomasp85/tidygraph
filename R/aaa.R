#' @importFrom igraph graph_attr graph_attr<-
`%gr_attr%` <- function(e1, e2) {
  graph_attr(e1) <- graph_attr(e2)
  attributes(e1) <- attributes(e2)
  e1
}

as_ind <- function(i, length) {
  seq_len(length)[i]
}

expect_influencer <- function() {
  if (!requireNamespace('influenceR', quietly = TRUE)) {
    stop('The `influenceR` package is required for this functionality')
  }
}
expect_netrankr <- function() {
  if (!requireNamespace('netrankr', quietly = TRUE)) {
    stop('The `netrankr` package is required for this functionality')
  }
}
expect_seriation <- function() {
  if (!requireNamespace('seriation', quietly = TRUE)) {
    stop('The `seriation` package is required for this functionality')
  }
}
expect_netswan <- function() {
  if (!requireNamespace('NetSwan', quietly = TRUE)) {
    stop('The `NetSwan` package is required for this functionality')
  }
}
