#' @describeIn tbl_graph Method to deal with Node objects from the data.tree package
#' @export
as_tbl_graph.Node <- function(x, directed = TRUE, mode = 'out', ...) {
  if (!requireNamespace("data.tree", quietly = TRUE)) {
    stop('The "data.tree" package is needed for this functionality to work', call. = FALSE)
  }
  direction <- if (mode == 'out') 'climb' else 'descend'
  data.tree::as.igraph.Node(x, directed = directed, direction = direction, ...)
}
