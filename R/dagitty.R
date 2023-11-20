#' @describeIn tbl_graph Method to deal with dagitty objects from the dagitty package
#' @export
as_tbl_graph.dagitty <- function(x, directed = TRUE, node_attr = NULL, edge_attr = 'beta', ...) {
  rlang::check_installed('dagitty', 'in order to coerce a dagitty object to tbl_graph')
  if(vctrs::vec_as_names(edge_attr) != edge_attr) stop('edge_attr must be a string of length > 0')
  coords <- dagitty::coordinates(x)
  nodes <- tibble::tibble(
    name = names(x),
    outcome = name %in% dagitty::outcomes(x),
    exposure = name %in% dagitty::exposures(x),
    latent = name %in% dagitty::latents(x),
    x = coords$x,
    y = coords$y
    )
  if (!is.null(node_attr)){
    if (vctrs::vec_as_names(node_attr) != node_attr) stop('node_attr must be a string of length > 0')
    nodes[node_attr] <- dagitty:::.vertexAttributes(x, node_attr)$a
  } 
  edges <- dagitty:::.edgeAttributes(x, edge_attr)
  names(edges) <- c('from', 'to', 'type', edge_attr)
  tbl_graph(nodes = nodes, edges = edges, directed = directed)
}
