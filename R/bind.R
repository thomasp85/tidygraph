#' Add graphs, nodes, or edges to a tbl_graph
#'
#' These functions are tbl_graph pendants to [dplyr::bind_rows()] that allows
#' you to grow your `tbl_graph` by adding rows to either the nodes data, the
#' edges data, or both. As with `bind_rows()` columns are matched by name and
#' are automatically filled with `NA` if the column doesn't exist in some
#' instances. In the case of `bind_graphs()` the graphs are automatically
#' converted to `tbl_graph` objects prior to binding. The edges in each graph
#' will continue to reference the nodes in the graph where they originated,
#' meaning that their terminal node indexes will be shifted to match the new
#' index of the node in the combined graph. This means the `bind_graphs()`
#' always result in a disconnected graph. See [graph_join()] for merging graphs
#' on common nodes.
#'
#' @param .data A `tbl_graph`
#'
#' @param ... In case of `bind_nodes()` and `bind_edges()` data.frames to add.
#' In the case of `bind_graphs()` objects that are convertible to `tbl_graph`
#' using `as_tbl_graph()`.
#'
#' @return A `tbl_graph` containing the new data
#'
#' @importFrom dplyr bind_rows
#' @importFrom tibble as_tibble
#' @export
#'
#' @examples
#' graph <- create_notable('bull')
#' new_graph <- create_notable('housex')
#'
#' # Add nodes
#' graph %>% bind_nodes(data.frame(new = 1:4))
#'
#' # Add edges
#' graph %>% bind_edges(data.frame(from = 1, to = 4:5))
#'
#' # Add graphs
#' graph %>% bind_graphs(new_graph)
#'
bind_graphs <- function(.data, ...) {
  stopifnot(is.tbl_graph(.data))
  dots <- lapply(list(...), as_tbl_graph)
  n_nodes <- sapply(dots, gorder)
  n_edges <- sapply(dots, gsize)
  offset <- rep(c(gorder(.data), gorder(.data) + cumsum(n_nodes)[-length(n_nodes)]), n_edges)
  nodes <- bind_rows(as_tibble(.data, active = 'nodes'), lapply(dots, as_tibble, active = 'nodes'))
  edges <- bind_rows(lapply(dots, as_tibble, active = 'edges'))
  edges$from <- edges$from + offset
  edges$to <- edges$to + offset
  edges <- bind_rows(as_tibble(.data, active = 'edges'), edges)
  as_tbl_graph(list(nodes = nodes, edges = edges)) %gr_attr% .data
}
#' @rdname bind_graphs
#' @importFrom igraph add_vertices
#' @importFrom dplyr bind_rows
#' @importFrom tibble as_tibble
#' @export
bind_nodes <- function(.data, ...) {
  stopifnot(is.tbl_graph(.data))
  d_tmp <- as_tibble(.data, acitve = 'nodes')
  new_nodes <- bind_rows(d_tmp, ...)
  .data <- add_vertices(.data, nrow(new_nodes) - nrow(d_tmp)) %gr_attr% .data
  set_graph_data(.data, new_nodes, active = 'nodes')
}
#' @rdname bind_graphs
#' @importFrom tibble as_tibble
#' @importFrom dplyr bind_rows
#' @importFrom igraph gorder add_edges
#' @export
bind_edges <- function(.data, ...) {
  stopifnot(is.tbl_graph(.data))
  d_tmp <- as_tibble(.data, active = 'edges')
  new_edges <- bind_rows(...)
  all_edges <- bind_rows(d_tmp, new_edges)
  if (any(is.na(all_edges$from)) || any(is.na(all_edges$to))) {
    stop('Edges can only be added if they contain a "to" and "from" node', call. = FALSE)
  }
  if (max(c(new_edges$to, new_edges$from)) > gorder(.data)) {
    stop('Edges can only be added if they refer to existing nodes', call. = FALSE)
  }
  .data <- add_edges(.data, rbind(new_edges$from, new_edges$to)) %gr_attr% .data
  set_graph_data(.data, all_edges, active = 'edges')
}
