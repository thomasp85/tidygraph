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
#' @importFrom dplyr bind_rows
#' @importFrom igraph add_edges graph_attr<- edge_attr<- vertex_attr<-
#' @noRd
#'
network_to_igraph <- function(graph) {
  metadata <- graph$gal
  metadata_names <- c("n", "directed", "hyper", "loops", "multiple", "bipartite", "mnext")
  graph_attrs <- metadata[!names(metadata) %in% metadata_names]
  if (metadata$hyper) {
    stop('Hypergraphs are currently unsupported', call. = FALSE)
  }
  
  node_attrs <- bind_rows(lapply(graph$val, `[`))
  node_attrs <- node_attrs[colnames(node_attrs) != "na"]
  names(node_attrs)[names(node_attrs) == "vertex.names"] <- "name"
  node_attrs <- node_attrs[, c("name", names(node_attrs)[names(node_attrs) != "name"])]
  
  if (is.numeric(metadata$bipartite)) {
    if ("type" %in% names(node_attrs)) {
      if (is.logical(node_attrs$type)) {
        warning('This network object is bipartite and uses a `logical` vertex attribute
                 named "type". igraph will assume that "type" is intended to label vertex 
                 modes in the resulting graph. Rename this vertex attribute if this is not 
                 intentional.')
        }
      if (!is.logical(node_attrs$type)) {
        stop('This network object is bipartite, but uses a non-`logical` vertex attribute 
              named "type". igraph uses a `logical` vertex attribute named "type" for 
              bipartite mapping. Rename this vertex attribute.', call. = FALSE)
      }
    }
    node_attrs$type <- c(rep(TRUE, metadata$bipartite), 
                         rep(FALSE, metadata$n - metadata$bipartite))
  }
  
  edge_attrs <- bind_rows(lapply(graph$mel, `[[`, "atl"))
  edge_attrs <- edge_attrs[colnames(edge_attrs) != "na"]
  
  outl <- vapply(graph$mel, `[[`, numeric(1), "outl")
  inl <- vapply(graph$mel, `[[`, numeric(1), "inl")
  if (metadata$directed) {
    el <- cbind(outl, inl)
  } else {
    el <- cbind(inl, outl)
  }
  el_vec <- as.vector(t(el))

  new_graph <- igraph::graph.empty(graph$gal$n, directed = isTRUE(graph$gal$directed))
  new_graph <- add_edges(new_graph, edges = el_vec)
  if (length(graph_attrs)) {
    graph_attr(new_graph) <- graph_attrs
  }
  if (nrow(node_attrs)) {
    vertex_attr(new_graph) <- node_attrs
  }
  if (nrow(edge_attrs)) {
    edge_attr(new_graph) <- edge_attrs
  }
  
  new_graph
}
