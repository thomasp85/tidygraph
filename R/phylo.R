#' @describeIn tbl_graph Method for handling phylo objects from the ape package
#' @importFrom igraph set_edge_attr
#' @export
as_tbl_graph.phylo <- function(x, directed = NULL, ...) {
  if (!requireNamespace("ape", quietly = TRUE)) {
    stop('The "ape" package is needed for this functionality to work', call. = FALSE)
  }
  if (is.null(directed)) directed <- ape::is.rooted(x)
  gr <- ape::as.igraph.phylo(x, directed = directed, ...)
  if (!is.null(x$edge.length)) gr <- set_edge_attr(gr, 'length', value = x$edge.length)
  as_tbl_graph(gr)
}

#' @describeIn tbl_graph Method for handling evonet objects from the ape package
#' @export
as_tbl_graph.evonet <- function(x, directed = TRUE, ...) {
  if (!requireNamespace("ape", quietly = TRUE)) {
    stop('The "ape" package is needed for this functionality to work', call. = FALSE)
  }
  if (is.null(directed)) directed <- ape::is.rooted(x)
  as_tbl_graph(ape::as.igraph.evonet(x, directed = directed, ...))
}
