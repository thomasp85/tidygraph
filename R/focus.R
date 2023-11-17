#' Select specific nodes or edges to compute on
#'
#' The `focus()`/`unfocus()` idiom allow you to temporarily tell tidygraph
#' algorithms to only calculate on a subset of the data, while keeping the full
#' graph intact. The purpose of this is to avoid having to calculate time
#' costly measures etc on all nodes or edges of a graph if only a few is needed.
#' E.g. you might only be interested in the shortest distance from one node to
#' another so rather than calculating this for all nodes you apply a focus on
#' one node and perform the calculation. It should be made clear that not all
#' algorithms will see a performance boost by being applied to a few nodes/edges
#' since their calculation is applied globally and the result for all
#' nodes/edges are provided in unison.
#'
#' @note focusing is the lowest prioritised operation on a graph. Applying a
#' [morph()] or a [group_by()] operation will unfocus the graph prior to
#' performing the operation. The same is true for the inverse operations
#' ([unmorph()] and [ungroup()]). Further, unfocusing will happen any time some
#' graph altering operation is performed, such as the `arrange()` and `slice()`
#' operations
#'
#' @inheritParams dplyr::filter
#'
#' @return A graph with focus applied
#'
#' @export
focus <- function(.data, ...) {
  UseMethod('focus')
}

#' @rdname focus
#' @export
focus.tbl_graph <- function(.data, ...) {
  .graph_context$set(.data)
  on.exit(.graph_context$clear())
  if (is.focused_tbl_graph(.data)) .data <- unfocus(.data)
  d_tmp <- as_tibble(.data)
  n_tmp <- nrow(d_tmp)
  d_tmp$.tidygraph_focus_index <- seq_len(n_tmp)
  d_tmp <- filter(d_tmp, ...)
  if (nrow(d_tmp) == 0) {
    cli::cli_inform("{.fun focus} didn't select any {active(.data)}. Returning unfocused graph")
    return(.data)
  }
  if (nrow(d_tmp) == n_tmp) {
    cli::cli_inform("{.fun focus} selected all {active(.data)}. Returning unfocused graph")
    return(.data)
  }
  apply_focus(.data, d_tmp$.tidygraph_focus_index)
}

#' @rdname focus
#' @export
focus.morphed_tbl_graph <- function(.data, ...) {
  .data[] <- lapply(.data, focus, ...)
  .data
}

#' @rdname focus
#' @export
unfocus <- function(.data, ...) {
  UseMethod('unfocus')
}

#' @rdname focus
#' @export
unfocus.tbl_graph <- function(.data, ...) {
  .data
}

#' @rdname focus
#' @export
unfocus.focused_tbl_graph <- function(.data, ...) {
  attr(.data, paste0(active(.data), '_focus_index')) <- NULL
  class(.data) <- setdiff(class(.data), 'focused_tbl_graph')
  .data
}

#' @rdname focus
#' @export
unfocus.morphed_tbl_graph <- function(.data, ...) {
  .data[] <- lapply(.data, unfocus, ...)
  .data
}

is.focused_tbl_graph <- function(x) inherits(x, 'focused_tbl_graph')

# HELPERS -----------------------------------------------------------------

apply_focus <- function(graph, index) {
  attr(graph, paste0(active(graph), '_focus_index')) <- index
  if (!is.focused_tbl_graph(graph)) {
    class(graph) <- c('focused_tbl_graph', class(graph))
  }
  graph
}
focus_ind <- function(x, active = NULL) {
  if (is.null(active)) active <- active(x)
  attr(x, paste0(active, '_focus_index')) %||%
    seq_len(if (active == "nodes") gorder(x) else gsize(x))
}
