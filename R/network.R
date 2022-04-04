#' @describeIn tbl_graph Method to handle network objects from the `network`
#' package. Requires this packages to work.
#' @export
as_tbl_graph.network <- function(x, ...) {
  as_tbl_graph(network_to_igraph(x))
}
#' Coerce network to igraph
#'
#' This utility function performs a conversion of network objects from the
#' network package into igraph object compatible with ggraph. Edge and node
#' attributes are preserved which, for the context of ggraph, is the most
#' important.
#'
#' @param graph A graph as a network object
#'
#' @return A representation of the same graph as given in the function call but
#' as an igraph object.
#'
#' @importFrom igraph graph_from_edgelist graph_attr<- edge_attr<- vertex_attr<-
#' @importFrom utils modifyList
#' @noRd
#'
network_to_igraph <- function(graph) {
  rlang::check_installed('network', 'in order to coerce network object to tbl_graph')
  graph_attr_names <- network::list.network.attributes(graph)
  graph_attr <- lapply(graph_attr_names, function(n) {
    network::get.network.attribute(graph, n)
  })
  names(graph_attr) <- graph_attr_names
  if (graph_attr$hyper) {
    cli::cli_abort('Hypergraphs are currently unsupported', call. = FALSE)
  }

  node_attr_names <- network::list.vertex.attributes(graph)
  node_attr <- lapply(node_attr_names, function(n) {
    network::get.vertex.attribute(graph, n)
  })
  names(node_attr) <- node_attr_names

  edge_attr_names <- network::list.edge.attributes(graph)
  edge_attr <- lapply(edge_attr_names, function(n) {
    network::get.edge.attribute(graph, n)
  })
  names(edge_attr) <- edge_attr_names

  edges <- as.matrix(graph, matrix.type = 'edgelist')
  class(edges) <- 'matrix'
  attributes(edges) <- attributes(edges)[c('dim', 'class')]

  new_graph <- graph_from_edgelist(edges, graph_attr$directed)
  graph_attr(new_graph) <- modifyList(
    graph_attr,
    list(bipartite = NULL, directed = NULL, hyper = NULL, loops = NULL,
         mnext = NULL, multiple = NULL, n = NULL)
  )
  if (is.character(node_attr$vertex.names)) {
    node_attr$name <- node_attr$vertex.names
  }
  node_attr$vertex.names <- NULL
  missing_nodes <- network::network.size(graph) - gorder(new_graph)
  new_graph <- add_vertices(new_graph, missing_nodes)
  vertex_attr(new_graph) <- node_attr
  edge_attr(new_graph) <- edge_attr

  new_graph
}
