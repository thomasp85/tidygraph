#' @describeIn tbl_graph Method for edge table
#' @export
#' @importFrom igraph graph_from_data_frame
as_tbl_graph.data.frame <- function(x, directed = TRUE, ...) {
  from_ind <- which(names(x) == 'from')
  if (length(from_ind) == 0) from_ind <- 1
  to_ind <- which(names(x) == 'to')
  if (length(to_ind) == 0) to_ind <- 2
  x <- x[, c(from_ind, to_ind, seq_along(x)[-c(from_ind, to_ind)]), drop = FALSE]
  graph <- graph_from_data_frame(x, directed = directed)
  as_tbl_graph(graph)
}

#' @export
as.data.frame.tbl_graph <- function(x, row.names = NULL, optional = FALSE, active = NULL, ...) {
  as.data.frame(as_tibble(x, active = active))
}
