focus <- function(.data, ...) {
  UseMethod('focus')
}
focus.tbl_graph <- function(.data, ...) {
  .graph_context$set(.data)
  on.exit(.graph_context$clear())
  if (is.focused_tbl_graph(.data)) .data <- unfocus(.data)
  d_tmp <- as_tibble(.data)
  d_tmp$.tidygraph_focus_index <- seq_len(nrow(d_tmp))
  d_tmp <- filter(d_tmp, ...)
  index <- d_tmp$.tidygraph_focus_index
  d_tmp$.tidygraph_focus_index <- NULL
  apply_focus(.data, index, d_tmp)
}
focus.grouped_tbl_graph <- function(.data, ...) {
  old_attr <- attributes(as_tibble(.data))
  .data <- NextMethod()
  new_attr <- regroup(as_tibble(.data), old_attr)
  apply_groups(.data, new_attr)
}
focus.morphed_tbl_graph <- function(.data, ...) {
  .data[] <- lapply(.data, focus, ...)
  .data
}
unfocus <- function(.data, ...) {
  UseMethod('unfocus')
}
unfocus.tbl_graph <- function(.data, ...) {
  .data
}
unfocus.focused_tbl_graph <- function(.data, ...) {
  attr(.data, paste0(active(.data), '_focus_index')) <- NULL
  class(.data) <- class(.data)[class(.data) != 'focused_tbl_graph']
  if (is.grouped_tbl_graph(.data)) {
    df <- as_tibble(.data)
    old_attr <- attributes(df)
    new_attr <- regroup(df, old_attr)
    .data <- apply_groups(.data, new_attr)
  }
  .data
}
unfocus.morphed_tbl_graph <- function(.data, ...) {
  .data[] <- lapply(.data, unfocus, ...)
  .data
}

is.focused_tbl_graph <- function(x) inherits(x, 'focused_tbl_graph')
# HELPERS -----------------------------------------------------------------

regroup <- function(data, attr) {
  vars <- lapply(attr$vars, function(v) as_quosure(sym(v)))
  group_by(data, !!! vars) %@% vars
}
apply_focus <- function(graph, index, tmp) {
  attr(graph, paste0(active(graph), '_focus_index')) <- index
  if (!is.focused_tbl_graph(graph)) {
    class(graph) <- c('focused_tbl_graph', class(graph))
  }
  graph
}
focus_ind <- function(x, active = NULL) {
  if (is.null(active)) active <- active(x)
  attr(x, paste0(active, '_focus_index'))
}
