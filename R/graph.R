#' @describeIn tbl_graph Method for handling graphNEL objects from the graph package (on Bioconductor)
#' @importFrom dplyr bind_rows
#' @importFrom tibble tibble as_tibble
#' @importFrom igraph as_edgelist edge_attr<- vertex_attr<- V
#' @export
as_tbl_graph.graphNEL <- function(x, ...) {
  rlang::check_installed('graph', 'in order to coerce graphNEL object to tbl_graph')
  directed <- graph::edgemode(x) == 'directed'
  adj_list <- lapply(graph::edgeL(x), `[[`, i = 'edges')
  graph <- as_tbl_graph(adj_list)

  edgelist <- as_edgelist(graph, names = TRUE)
  edge_names <- apply(edgelist, 1, paste, collapse = '|')
  graph_ed <- graph::edgeData(x)
  edge_data <- bind_rows(lapply(graph_ed, as_tibble))
  edge_data_full <- edge_data[rep(nrow(edge_data) + 1, length(edge_names)), ]
  edge_data_full[match(names(graph_ed), edge_names), ] <- edge_data

  graph_nd <- graph::nodeData(x)
  graph_nd <- lapply(names(graph_nd), function(n) {
    tibble(name = n, !!!graph_nd[[n]])
  })
  node_data <- bind_rows(graph_nd)
  node_data <- node_data[match(node_data$name, V(graph)$name), ]

  edge_attr(graph) <- edge_data_full
  vertex_attr(graph) <- node_data
  as_tbl_graph(graph)
}
#' @describeIn tbl_graph Method for handling graphAM objects from the graph package (on Bioconductor)
#' @export
as_tbl_graph.graphAM <- function(x, ...) {
  rlang::check_installed('methods', 'in order to coerce graphAM object to tbl_graph')
  as_tbl_graph(methods::as(x, 'graphNEL'), ...)
}
#' @describeIn tbl_graph Method for handling graphBAM objects from the graph package (on Bioconductor)
#' @export
as_tbl_graph.graphBAM <- function(x, ...) {
  rlang::check_installed('methods', 'in order to coerce graphBAM object to tbl_graph')
  as_tbl_graph(methods::as(x, 'graphNEL'), ...)
}
