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
#' @param .data A `tbl_graph`, or a list of `tbl_graph` objects (for
#' `bind_graphs()`).
#'
#' @param ... In case of `bind_nodes()` and `bind_edges()` data.frames to add.
#' In the case of `bind_graphs()` objects that are convertible to `tbl_graph`
#' using `as_tbl_graph()`.
#'
#' @param node_key The name of the column in `nodes` that character represented
#' `to` and `from` columns should be matched against. If `NA` the first column
#' is always chosen. This setting has no effect if `to` and `from` are given as
#' integers.
#'
#' @return A `tbl_graph` containing the new data
#'
#' @importFrom dplyr bind_rows
#' @importFrom tibble as_tibble
#' @importFrom igraph is_directed
#' @importFrom rlang is_bare_list list2
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
  .data <- unfocus(.data)
  if (is_bare_list(.data)) {
    .data <- lapply(c(.data, list2(...)), as_tbl_graph)
    dots <- .data[-1]
    .data <- .data[[1]]
  } else {
    .data <- as_tbl_graph(.data)
    dots <- lapply(list2(...), as_tbl_graph)
  }
  if (length(dots) == 0) return(.data)
  n_nodes <- sapply(dots, gorder)
  n_edges <- sapply(dots, gsize)
  offset <- rep(c(gorder(.data), gorder(.data) + cumsum(n_nodes)[-length(n_nodes)]), n_edges)
  nodes <- bind_rows(as_tibble(.data, active = 'nodes'), lapply(dots, as_tibble, active = 'nodes'))
  edges <- bind_rows(lapply(dots, as_tibble, active = 'edges'))
  edges$from <- edges$from + offset
  edges$to <- edges$to + offset
  edges <- bind_rows(as_tibble(.data, active = 'edges'), edges)
  as_tbl_graph(list(nodes = nodes, edges = edges), directed = is_directed(.data)) %gr_attr% .data
}
#' @rdname bind_graphs
#' @importFrom igraph add_vertices
#' @importFrom dplyr bind_rows
#' @importFrom tibble as_tibble
#' @export
bind_nodes <- function(.data, ...) {
  .data <- unfocus(.data)
  check_tbl_graph(.data)
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
bind_edges <- function(.data, ..., node_key = 'name') {
  .data <- unfocus(.data)
  check_tbl_graph(.data)
  d_tmp <- as_tibble(.data, active = 'edges')
  nodes <- as_tibble(.data, active = 'nodes')
  if (is.na(node_key)) {
    name_ind <- 1L
  } else {
    name_ind <- which(names(nodes) == node_key)
    if (length(name_ind) == 0) name_ind <- 1
  }
  new_edges <- bind_rows(...)
  if (!all(c('to', 'from') %in% names(new_edges))) {
    cli::cli_abort('Edges can only be added if they contain a {.col to} and {.col from} column')
  }
  if (is.character(new_edges$from)) {
    new_edges$from <- match(new_edges$from, nodes[[name_ind]])
  }
  if (is.character(new_edges$to)) {
    new_edges$to <- match(new_edges$to, nodes[[name_ind]])
  }
  all_edges <- bind_rows(d_tmp, new_edges)
  if (any(is.na(all_edges$from)) || any(is.na(all_edges$to))) {
    cli::cli_abort('Edges can only be added if they contain a valid {.col to} and {.col from} column')
  }
  if (max(c(new_edges$to, new_edges$from)) > gorder(.data)) {
    cli::cli_abort('Edges can only be added if they refer to existing nodes')
  }
  .data <- add_edges(.data, rbind(new_edges$from, new_edges$to)) %gr_attr% .data
  set_graph_data(.data, all_edges, active = 'edges')
}
