#' Change terminal nodes of edges
#'
#' The reroute verb lets you change the beginning and end node of edges by
#' specifying the new indexes of the start and/or end node(s). Optionally only
#' a subset of the edges can be rerouted using the subset argument, which should
#' be an expression that are to be evaluated in the context of the edge data and
#' should return an index compliant vector (either logical or integer).
#'
#' @param .data A tbl_graph or morphed_tbl_graph object. grouped_tbl_graph will
#' be ungrouped prior to rerouting
#' @param from,to The new indexes of the terminal nodes. If `NULL` nothing will
#' be changed
#' @param subset An expression evaluating to an indexing vector in the context
#' of the edge data. If `NULL` it will use focused edges if available or all
#' edges
#'
#' @return An object of the same class as .data
#' @export
#'
#' @examples
#' # Switch direction of edges
#' create_notable('meredith') %>%
#'   activate(edges) %>%
#'   reroute(from = to, to = from)
#'
#' # Using subset
#' create_notable('meredith') %>%
#'   activate(edges) %>%
#'   reroute(from = 1, subset = to > 10)
reroute <- function(.data, from = NULL, to = NULL, subset = NULL) {
  UseMethod('reroute')
}
#' @export
#' @importFrom rlang enquo eval_tidy
#' @importFrom igraph is_directed
reroute.tbl_graph <- function(.data, from = NULL, to = NULL, subset = NULL) {
  .register_graph_context(.data)
  expect_edges()
  from <- enquo(from)
  to <- enquo(to)
  if (is.grouped_tbl_graph(.data)) {
    cli::cli_inform('Ungrouping prior to rerouting edges')
    .data <- ungroup(.data)
  }
  edges <- as_tibble(.data, active = 'edges')
  subset <- enquo(subset)
  subset <- eval_tidy(subset, edges)
  if (is.null(subset)) subset <- focus_ind(.data, 'edges')
  edges_sub <- edges[subset, , drop = FALSE]
  from <- eval_tidy(from, edges_sub)
  if (!is.null(from)) edges$from[subset] <- rep(from, length.out = nrow(edges_sub))
  to <- eval_tidy(to, edges_sub)
  if (!is.null(to)) edges$to[subset] <- rep(to, length.out = nrow(edges_sub))
  .data <- tbl_graph(
    nodes = as_tibble(.data, active = 'nodes'),
    edges = edges,
    directed = is_directed(.data)
  ) %gr_attr% .data
  active(.data) <- 'edges'
  .data
}
#' @export
#' @importFrom rlang enquo
reroute.morphed_tbl_graph <- function(.data, from = NULL, to = NULL, subset = NULL) {
  from <- enquo(from)
  to <- enquo(to)
  .data[] <- lapply(.data, reroute, from = !!from, to = !!to, subset = subset)
  .data
}
