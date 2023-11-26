#' @describeIn tbl_graph Method to deal with dagitty objects from the dagitty package
#' @export
#' @importFrom rlang is_empty check_installed
as_tbl_graph.dagitty <- function(x, directed = TRUE, ...) {
  check_installed('dagitty', 'in order to coerce a dagitty object to tbl_graph')
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
  coords <- dagitty::coordinates(x)
  if (!all(is.na(coords$x))) {
    nodes$x <- coords$x
    nodes$y <- coords$y
  }
  
  e <- dagitty::edges(x)
  if (is_empty(e)){
    e <- tibble::tibble(from = integer(), to = integer())
  } else {
    e <- e[c('v', 'w', 'e')]
    names(e) <- c('from', 'to', 'type')
  } 
  
  tbl_graph(nodes = nodes, edges = e, directed = directed)
}
