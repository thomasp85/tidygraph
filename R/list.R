#' @describeIn tbl_graph Method for adjacency lists and lists of node and edge tables
#' @export
as_tbl_graph.list <- function(x, directed = TRUE, ...) {
  graph <- switch(
    guess_list_type(x),
    adjacency = as_graph_adj_list(x, directed = directed),
    node_edge = as_graph_node_edge(x, directed = directed),
    unknown = stop("Unknown list format", call. = FALSE)
  )
  as_tbl_graph(graph)
}
#' @export
as.list.tbl_graph <- function(x, ...) {
  list(
    nodes = as_tibble(x, active = 'nodes'),
    edges = as_tibble(x, active = 'edges')
  )
}

guess_list_type <- function(x) {
  if (length(x) == 2 &&
      any(names(x) %in% c('nodes', 'vertices')) &&
      any(names(x) %in% c('edges', 'links'))) {
    return('node_edge')
  }
  elements <- sapply(x, function(el) class(el)[1])
  if (all(elements == 'character') &&
      all(unlist(x) %in% names(x))) {
    return('adjacency')
  }
  if (any(elements %in% c('numeric'))) {
    x <- lapply(x, as.integer)
    elements[] <- 'integer'
  }
  if (all(elements == 'integer') &&
      !anyNA(unlist(x)) &&
      max(unlist(x)) <= length(x) &&
      min(unlist(x)) >= 0) {
    return('adjacency')
  }
  'unknown'
}

#' @importFrom igraph graph_from_adj_list set_vertex_attr
as_graph_adj_list <- function(x, directed) {
  if (inherits(x[[1]], 'character')) {
    x <- split(match(unlist(x), names(x)), rep(names(x), lengths(x)))
  }
  if (any(unlist(x) == 0)) {
    x <- lapply(x, `+`, 1)
  }
  gr <- graph_from_adj_list(unname(x), mode = if (directed) 'out' else 'all')
  if (!is.null(names(x))) {
    gr <- set_vertex_attr(gr, 'name', value = names(x))
  }
  gr
}

#' @importFrom igraph graph_from_edgelist vertex_attr<- add_vertices gorder
#' @importFrom tibble tibble
as_graph_node_edge <- function(x, directed) {
  nodes <- x[[which(names(x) %in% c('nodes', 'vertices'))]]
  edges <- x[[which(names(x) %in% c('edges', 'links'))]]
  if (is.null(edges)) {
    edges <- tibble(from = integer(), to = integer())
  }
  from_ind <- which(names(edges) == 'from')
  if (length(from_ind) == 0) from_ind <- 1
  to_ind <- which(names(edges) == 'to')
  if (length(to_ind) == 0) to_ind <- 2
  name_ind <- which(names(nodes) == 'name')
  if (length(name_ind) == 0) name_ind <- 1
  edges <- edges[, c(from_ind, to_ind, seq_along(edges)[-c(from_ind, to_ind)]), drop = FALSE]
  if (is.character(edges[, 1])) {
    edges[, 1] <- match(edges[, 1], nodes[, name_ind])
  }
  if (is.character(edges[, 2])) {
    edges[, 2] <- match(edges[, 2], nodes[, name_ind])
  }
  gr <- graph_from_edgelist(as.matrix(edges[, 1:2]), directed = directed)
  edge_attr(gr) <- as.list(edges[, -c(1:2), drop = FALSE])
  if (gorder(gr) != nrow(nodes)) {
    gr <- add_vertices(gr, nrow(nodes) - gorder(gr))
  }
  vertex_attr(gr) <- as.list(nodes)
  gr
}
