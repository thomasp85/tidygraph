#' A data structure for tidy graph manipulation
#'
#' The `tbl_graph` class is a thin wrapper around an `igraph` object that
#' provides methods for manipulating the graph using the tidy API. As it is just
#' a subclass of `igraph` every igraph method will work as expected. A
#' `grouped_tbl_graph` is the equivalent of a `grouped_df` where either the
#' nodes or the edges has been grouped. The `grouped_tbl_graph` is not
#' constructed directly but by using the [group_by()] verb. After creation of a
#' `tbl_graph` the nodes are activated by default. The context can be changed
#' using the [activate()] verb and affects all subsequent operations. Changing
#' context automatically drops any grouping. The current active context can
#' always be extracted with [as_tibble()], which drops the graph structure and
#' just returns a `tbl_df` or a `grouped_df` depending on the state of the
#' `tbl_graph`. The returned context can be overriden by using the `active`
#' argument in [as_tibble()].
#'
#' @details
#' Constructors are provided for most data structures that resembles networks.
#' If a class provides an [igraph::as.igraph()] method it is automatically
#' supported.
#'
#' @param nodes A `data.frame` containing information about the nodes in the
#' graph. If `edges$to` and/or `edges$from` are characters then they will be
#' matched to the column named according to `node_key` in nodes, if it exists.
#' If not, they will be matched to the first column.
#'
#' @param edges A `data.frame` containing information about the edges in the
#' graph. The terminal nodes of each edge must either be encoded in a `to` and
#' `from` column, or in the two first columns, as integers. These integers refer to
#' `nodes` index.
#'
#' @param x An object convertible to a `tbl_graph`
#'
#' @param directed Should the constructed graph be directed (defaults to `TRUE`)
#'
#' @param node_key The name of the column in `nodes` that character represented
#' `to` and `from` columns should be matched against. If `NA` the first column
#' is always chosen. This setting has no effect if `to` and `from` are given as
#' integers.
#'
#' @param mode In case `directed = TRUE` should the edge direction be away from
#' node or towards. Possible values are `"out"` (default) or `"in"`.
#'
#' @param ... Arguments passed on to the conversion function
#'
#' @return A `tbl_graph` object
#'
#' @examples
#' rstat_nodes <- data.frame(name = c("Hadley", "David", "Romain", "Julia"))
#' rstat_edges <- data.frame(from = c(1, 1, 1, 2, 3, 3, 4, 4, 4),
#'                             to = c(2, 3, 4, 1, 1, 2, 1, 2, 3))
#' tbl_graph(nodes = rstat_nodes, edges = rstat_edges)
#' @export
#'
tbl_graph <- function(nodes = NULL, edges = NULL, directed = TRUE, node_key = 'name') {
  as_tbl_graph(list(nodes = nodes, edges = edges), directed = directed, node_key = node_key)
}
#' @rdname tbl_graph
#' @export
as_tbl_graph <- function(x, ...) {
  UseMethod('as_tbl_graph')
}
#' @describeIn tbl_graph Default method. tries to call [igraph::as.igraph()] on the input.
#' @export
#' @importFrom igraph as.igraph
as_tbl_graph.default <- function(x, ...) {
  rlang::try_fetch(
    as_tbl_graph(as.igraph(x)),
    error = function(cnd) cli::cli_abort('No support for {.cls {class(x)}} objects', parent = cnd)
  )
}
#' @rdname tbl_graph
#' @export
is.tbl_graph <- function(x) {
  inherits(x, 'tbl_graph')
}

#' @importFrom rlang caller_arg
check_tbl_graph <- function(x, arg = caller_arg(x), call = caller_env()) {
  if (!is.tbl_graph(x)) {
    cli::cli_abort('{.arg {arg}} must be a {.cls tbl_graph} object', call = call)
  }
}

check_reserved <- function(x, call = caller_env()) {
  if (any(names(x) == '.tbl_graph_index')) {
    cli::cli_abort('The attribute name {.field .tbl_graph_index} is reserved', call = call)
  }
}

new_name_tibble <- function(x, active = NULL, name = "A tibble", suffix = "") {
  x <- as_tibble(x, active, focused = FALSE)
  attr(x, "name") <- name
  attr(x, "suffix") <- suffix
  class(x) <- c("named_tbl", class(x))
  x
}
#' @importFrom pillar tbl_sum
#' @export
tbl_sum.named_tbl <- function(x) {
  summary <- NextMethod()
  names(summary)[1] <- attr(x, "name")
  summary[1] <- paste0(summary[1], attr(x, "suffix"))
  summary
}
#' @importFrom pillar tbl_format_footer
#' @export
tbl_format_footer.named_tbl <- function(x, setup, ...) {
  footer <- NextMethod()
  footer[!grepl("to see more rows", footer)]
}

#' @importFrom tools toTitleCase
#' @importFrom rlang as_quosure sym
#' @export
print.tbl_graph <- function(x, ..., n_non_active = 3) {
  graph_desc <- describe_graph(x)
  not_active <- if (active(x) == 'nodes') 'edges' else 'nodes'
  top <- toTitleCase(paste0(substr(active(x), 1, 4), ' data'))
  bottom <- toTitleCase(paste0(substr(not_active, 1, 4), ' data'))
  cat_subtle('# A tbl_graph: ', gorder(x), ' nodes and ', gsize(x), ' edges\n', sep = '')
  cat_subtle('#\n')
  cat_subtle('# ', graph_desc, '\n', sep = '')
  cat_subtle('#\n')
  if (is.focused_tbl_graph(x)) {
    cat_subtle('# Focused on ', length(focus_ind(x)), ' ', active(x), '\n')
  }
  print(new_name_tibble(x, NULL, top, " (active)"), ...)
  cat_subtle('#\n')
  print(new_name_tibble(x, not_active, bottom, ""), n = n_non_active)
  invisible(x)
}

#' @importFrom pillar glimpse
#' @export
glimpse.tbl_graph <- function(x, width = NULL, ...) {
  cli::cli_rule(left = "Nodes")
  glimpse(as_tibble(x, active = "nodes"))
  cli::cat_line()
  cli::cli_rule(left = "Edges")
  glimpse(as_tibble(x, active = "edges"))
}

#' @importFrom pillar glimpse
#' @export
glimpse.morphed_tbl_graph <- function(x, width = NULL, ...) {
  graph <- attr(x, '.orig_graph')

  cat_subtle("Currently morphed to a ", gsub('_', ' ', sub('to_', '', attr(x, '.morpher'))), " representation\n")
  cli::cat_line()
  cli::cli_rule(left = "Nodes")
  glimpse(as_tibble(graph, active = "nodes"))
  cli::cat_line()
  cli::cli_rule(left = "Edges")
  glimpse(as_tibble(graph, active = "edges"))
}

#' @importFrom pillar style_subtle
cat_subtle <- function(...) cat(pillar::style_subtle(paste0(...)))

#' @export
print.morphed_tbl_graph <- function(x, ...) {
  graph <- attr(x, '.orig_graph')
  cat('# A tbl_graph temporarily morphed to a ', gsub('_', ' ', sub('to_', '', attr(x, '.morpher'))), ' representation\n', sep = '')
  cat('# \n')
  cat('# Original graph is ', tolower(describe_graph(graph)), '\n', sep = '')
  cat('# consisting of ', gorder(graph), ' nodes and ', gsize(graph), ' edges\n', sep = '')
}
#' @importFrom igraph is_simple is_directed is_bipartite is_connected is_dag gorder
describe_graph <- function(x) {
  if (gorder(x) == 0) return('An empty graph')
  prop <- list(simple = is_simple(x), directed = is_directed(x),
                  bipartite = is_bipartite(x), connected = is_connected(x),
                  tree = is_tree(x), forest = is_forest(x), DAG = is_dag(x))
  desc <- c()
  if (prop$tree || prop$forest) {
    desc[1] <- if (prop$directed) 'A rooted' else 'An unrooted'
    desc[2] <- if (prop$tree) 'tree' else paste0('forest with ', count_components(x), ' trees')
  } else {
    desc[1] <- if (prop$DAG) 'A directed acyclic' else if (prop$bipartite) 'A bipartite' else if (prop$directed) 'A directed' else 'An undirected'
    desc[2] <- if (prop$simple) 'simple graph' else 'multigraph'
    n_comp <- count_components(x)
    desc[3] <- paste0('with ' , n_comp, ' component', if (n_comp > 1) 's' else '')
  }
  paste(desc, collapse = ' ')
}
#' @importFrom igraph is_connected is_simple gorder gsize is_directed
is_tree <- function(x) {
  is_connected(x) && is_simple(x) && (gorder(x) - gsize(x) == 1)
}
#' @importFrom igraph is_connected is_simple gorder gsize count_components is_directed
is_forest <- function(x) {
  !is_connected(x) && is_simple(x) && (gorder(x) - gsize(x) - count_components(x) == 0)
}
#' @export
as_tbl_graph.tbl_graph <- function(x, ...) {
  x
}
set_graph_data <- function(x, value, active) {
  UseMethod('set_graph_data')
}
#' @export
set_graph_data.tbl_graph <- function(x, value, active = NULL) {
  if (is.null(active)) active <- active(x)
  switch(
    active,
    nodes = set_node_attributes(x, value),
    edges = set_edge_attributes(x, value),
    cli::cli_abort('Unknown active element: {.val {active}}. Only nodes and edges supported')
  )
}
#' @export
set_graph_data.grouped_tbl_graph <- function(x, value, active = NULL) {
  x <- NextMethod()
  apply_groups(x, value)
}
#' @importFrom igraph vertex_attr<-
set_node_attributes <- function(x, value) {
  if (is.focused_tbl_graph(x)) {
    value <- merge_into(value, as_tibble(x, active = 'nodes', focused = FALSE), focus_ind(x, 'nodes'))
  }
  vertex_attr(x) <- as.list(value)
  x
}
#' @importFrom igraph edge_attr<-
set_edge_attributes <- function(x, value) {
  if (is.focused_tbl_graph(x)) {
    value <- merge_into(value, as_tibble(x, active = 'edges', focused = FALSE), focus_ind(x, 'edges'))
  }
  value <- value[, !names(value) %in% c('from', 'to')]
  edge_attr(x) <- as.list(value)
  x
}
#' @importFrom dplyr tbl_vars
#' @export
tbl_vars.tbl_graph <- function(x) {
  tbl_vars(as_tibble(x))
}
#' @export
dplyr::tbl_vars

merge_into <- function(new, old, index) {
  order <- new[integer(0), , drop = FALSE]
  old <- bind_rows(order, old)
  old[, !names(old) %in% names(new)] <- NULL
  old[index, ] <- new
  old
}
