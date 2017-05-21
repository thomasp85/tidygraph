#' @describeIn tbl_graph Method for hclust objects
#' @importFrom stats as.dendrogram
#' @export
as_tbl_graph.hclust <- function(x, directed = TRUE, mode = 'out', ...) {
  as_tbl_graph(as.dendrogram(x), directed, mode)
}
