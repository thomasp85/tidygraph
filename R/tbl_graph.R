#' A data structure for tidy graph manipulation
#'
#' The `tbl_graph` class is a thin wrapper around an `igraph` object that
#' provides methods for manipulating the graph using the tidy API. As it is just
#' a subclass of `igraph` every igraph method will work as expected. A
#' `grouped_tbl_graph` is the equivalent of a `grouped_df` where either the
#' nodes or the edges has been grouped. The `grouped_tbl_graph` is not
#' constructed directly but by using the [group_by()] verb. After creation of a
#' `tble_graph` the nodes are activated by default. The context can be changed
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
#' @param x An object convertible to a `tbl_graph`
#'
#' @param directed Should the constructed graph be directed (defaults to `TRUE`)
#'
#' @param mode In case `directed = TRUE` should the edge direction be away from
#' node or towards. Possible values are `"out"` (default) or `"in"`.
#'
#' @param ... Arguments passed on to the conversion function
#'
#' @return A `tbl_graph` object
#'
#' @aliases tbl_graph
#' @export
#'
as_tbl_graph <- function(x, ...) {
  UseMethod('as_tbl_graph')
}
#' @describeIn as_tbl_graph Default method. tries to call [igraph::as.igraph()] on the input.
#' @export
#' @importFrom igraph as.igraph
as_tbl_graph.default <- function(x, ...) {
  tryCatch({
    as_tbl_graph(as.igraph(x))
  }, error = function(e) stop('No support for ', class(x)[1], ' objects', call. = FALSE))
}
#' @importFrom tibble trunc_mat
#' @importFrom tools toTitleCase
#' @importFrom igraph is_simple is_directed is_bipartite is_connected is_chordal is_dag
#' @export
print.tbl_graph <- function(x, ...) {
  arg_list <- list(...)
  tree <- is_tree(x)
  properties <- c(simple = is_simple(x), directed = is_directed(x),
                  undirected = !is_directed(x), bipartite = is_bipartite(x),
                  connected = is_connected(x), tree = tree,
                  DAG = !tree && is_dag(x))
  properties <- names(properties)[properties]
  not_active <- if (active(x) == 'nodes') 'edges' else 'nodes'
  top <- do.call(trunc_mat, modifyList(arg_list, list(x = as_tibble(x), n = 6)))
  top$summary <- sub('A tibble', toTitleCase(paste0(substr(active(x), 1, 4), ' data')), top$summary)
  top$summary <- paste0(top$summary, ' (active)')
  bottom <- do.call(trunc_mat, modifyList(arg_list, list(x = as_tibble(x, active = not_active), n = 3)))
  bottom$summary <- sub('A tibble', toTitleCase(paste0(substr(not_active, 1, 4), ' data')), bottom$summary)
  cat('# A tbl_graph: ', gorder(x), ' nodes and ', gsize(x), ' edges\n', sep = '')
  cat('#\n')
  cat('# Properties: ', paste(properties, collapse = ', '), '\n', sep = '')
  cat('#\n')
  print(top)
  cat('#\n')
  print(bottom)
  invisible(x)
}
#' @importFrom igraph is_connected
is_tree <- function(x) {
  is_connected(x) && (gorder(x) - gsize(x) == 1)
}
#' @export
as_tbl_graph.tbl_graph <- function(x, ...) {
  x
}
#' @importFrom igraph as.igraph
#' @export
as.igraph.tbl_graph <- function(x, ...) {
  class(x) <- 'igraph'
  attr(x, 'active') <- NULL
  x
}
#' @importFrom dplyr tbl_vars
#' @export
tbl_vars.tbl_graph <- function(x) {
  names(as_tibble(x))
}
#' @importFrom dplyr groups
#' @export
groups.tbl_graph <- function(x) {
  NULL
}
