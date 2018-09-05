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
#' @importFrom igraph graph.empty graph_attr<- edge_attr<- vertex_attr<-
#' @noRd
#'
network_to_igraph <- function(graph) {
  metadata <- graph$gal
  # graph-level "attributes" to drop as they're better described as metadata
  metadata_names <- c("n", "directed", "hyper", "loops", "multiple", "bipartite", "mnext")
  graph_attrs <- metadata[!names(metadata) %in% metadata_names]
  if (metadata$hyper) {
    stop('Hypergraphs are currently unsupported', call. = FALSE)
  }
  
  node_attrs <- bind_rows(lapply(graph$val, `[`))
  # {igraph} doesn't track "missing"/"na" vertices.
  node_attrs <- node_attrs[colnames(node_attrs) != "na"]
  # {network} assumes vertex names are assigned to an attribute called "vertex.names"
  # {igraph} assumes they are assigned to an attribute called "name"
  names(node_attrs)[names(node_attrs) == "vertex.names"] <- "name"
  # both packages have instances where they expect vertex names to be their first attribute
  node_attrs <- node_attrs[, c("name", names(node_attrs)[names(node_attrs) != "name"])]
  
  # "bipartite" networks are the weak spot:
  #
  # Both packages are problematic in conflating bipartite and two-mode networks, but do
  # so in different ways. They are also both insufficiently strict about handling graphs 
  # that are inherently not bipartite (e.g. they allow bipartite networks to have loops).
  #
  # {network} tracks the number of vertices belonging to the "actors" mode as a graph-level 
  # attribute: `<network-object>$gal$bipartite`.
  # 
  # The count of vertices of the "non-actors" mode (e.g events, locations, etc.) are then 
  # `<network-object>$gal$n - <network-object>$gal$bipartite`.
  # 
  # If `<network-object>$gal$bipartite` is numeric, it's considered "bipartite".
  # If it's `FALSE` OR `NULL`, it's not. 
  #
  # This is in conflict with {igraph}, which explicitly uses a `logical` vertex attribute
  # to track vertex modes, conventionally named "type".
  # 
  # The solution I've implemented in my own package is to assign "actors" (vertices with 
  # IDs < `<network-object>$gal$bipartite`) as `TRUE`.
  # This simplifies things when converting back to network-class objects as the "type"
  # attribute can then be interpreted as "is_actor". This facilitates consistently accurate
  # round-trip conversions.
  if (is.numeric(metadata$bipartite)) {
    if ("type" %in% names(node_attrs)) {
      if(is.logical(node_attrs$type)) {
        warning('This network object is bipartite and uses a `logical` vertex attribute
                 named "type". igraph will assume that "type" is intended to label vertex 
                 modes in the resulting graph. Rename this vertex attribute if this is not 
                 intentional.')
        }
      if(!is.logical(node_attrs$type)) {
        stop('This network object is bipartite, but uses a non-`logical` vertex attribute 
              named "type". igraph uses a `logical` vertex attribute named "type" for 
              bipartite mapping. Rename this vertex attribute.', call. = FALSE)
      }
    }
    node_attrs$type <- c(rep(TRUE, metadata$bipartite), 
                         rep(FALSE, metadata$n - metadata$bipartite))
  }
  
  edge_attrs <- bind_rows(lapply(graph$mel, `[[`, "atl"))
  # {igraph} doesn't track "missing"/"na" edges.
  edge_attrs <- edge_attrs[colnames(edge_attrs) != "na"]
  
  outl <- vapply(graph$mel, `[[`, numeric(1), "outl") # may be integer or double
  inl <- vapply(graph$mel, `[[`, numeric(1), "inl")
  if (metadata$directed) {
    el <- cbind(outl, inl)
  } else {
    # {network} reverses egos and alters for undirected edge lists. Controling this allows
    # validation of round-trip conversions.
    el <- cbind(inl, outl)
  }
  # vectorizing edges allows them to be added to an empty graph
  el_vec <- as.vector(t(el))
  # starting with an empty graph allows for isolates
  new_graph <- graph.empty(graph$gal$n, directed = graph$gal$directed)
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
