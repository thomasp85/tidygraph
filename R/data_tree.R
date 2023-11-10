#' @describeIn tbl_graph Method to deal with Node objects from the data.tree package
#' @export
as_tbl_graph.Node <- function(x, directed = TRUE, mode = 'out', ...) {
  rlang::check_installed('data.tree', 'in order to coerce Node object to tbl_graph')
  direction <- if (mode == 'out') 'climb' else 'descend'
  as_tbl_graph(data.tree::as.igraph.Node(x, directed = directed, direction = direction, ...))
}
