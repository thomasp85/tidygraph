#' @describeIn tbl_graph Method to deal with dagitty objects from the dagitty package
#' @export
#' @importFrom rlang is_empty check_installed
as_tbl_graph.dagitty <- function(x, directed = TRUE, node_attr = NULL, edge_attr = NULL, ...) {
  check_installed('dagitty', 'in order to coerce a dagitty object to tbl_graph')
  if (!is_empty(intersect(node_attr, c('adjusted', 'latent', 'exposure', 'outcome')))) stop('node_attr cannot be adjusted, exposure, outcome, or latent, as these are automatically added if present')
  if (is_empty(names(x))) return(create_empty(0))
  
  nodes <- tibble::tibble(
    name = names(x)
  )
  adjusted <- dagitty::adjustedNodes(x)
  if (!is_empty(adjusted)) nodes$adjusted <- nodes$name %in% adjusted
  exposure <- dagitty::exposures(x)
  if (!is_empty(exposure)) nodes$exposure <- nodes$name %in% exposure
  outcome <- dagitty::outcomes(x)
  if (!is_empty(outcome)) nodes$outcome <- nodes$name %in% outcome
  latent <- dagitty::latents(x)
  if (!is_empty(latent)) nodes$latent <- nodes$name %in% latent
  coords <- coordinates(x)
  if (!all(is.na(coords$x))) {
    nodes$x <- coords$x
    nodes$y <- coords$y
  }
  
  for (a in node_attr){
    if (vctrs::vec_as_names(a, repair = 'unique') != a) stop('each node_attr must be a string of length > 0')
    nodes[a] <- dagitty:::.vertexAttributes(x, a)$a
  }
  
  edges <- dagitty::edges(x)
  if (is_empty(edges)){
    edges <- tibble::tibble(from = int(), to = int())
  } else {
    edges <- edges[c('v', 'w', 'e')]
    names(edges) <- c('from', 'to', 'type')
    for (a in edge_attr){
      if (vctrs::vec_as_names(a, repair = 'unique') != a) stop('each edge_attr must be a string of length > 0')
      edges[a] <- dagitty:::.edgeAttributes(x, a)$a
    }
  } 
  
  tbl_graph(nodes = nodes, edges = edges, directed = directed)
}
