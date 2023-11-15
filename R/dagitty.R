#' @describeIn tbl_graph Method to deal with dagitty objects from the dagitty package
#' @export
as_tbl_graph.dagitty <- function(x, directed = TRUE, ...) {
  rlang::check_installed('dagitty', 'in order to coerce a dagitty object to tbl_graph')
  nodes <- data.frame(name = names(x))
  edges <- dagitty::edges(x)[1:2]
  names(edges) <- c('from', 'to')
  tbl_graph(nodes = nodes, edges = edges, directed = directed)
}
