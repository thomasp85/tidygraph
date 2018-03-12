#' @importFrom dplyr group_by
#' @export
group_by.tbl_graph <- function(.data, ..., add = FALSE) {
  .register_graph_context(.data)
  d_tmp <- as_tibble(.data)
  d_tmp <- group_by(d_tmp, ..., add = add)
  apply_groups(.data, attributes(d_tmp))
}
#' @export
#' @importFrom dplyr group_by
group_by.morphed_tbl_graph <- function(.data, ...) {
  .data[] <- lapply(.data, group_by, ...)
  .data
}
#' @export
dplyr::group_by

#' @importFrom dplyr ungroup
#' @export
ungroup.tbl_graph <- function(x, ...) {
  x
}
#' @importFrom dplyr ungroup
#' @export
ungroup.grouped_tbl_graph <- function(x, ...) {
  attr(x, paste0(active(x), '_group_attr')) <- NULL
  class(x) <- class(x)[class(x) != 'grouped_tbl_graph']
  x
}
#' @export
#' @importFrom dplyr ungroup
ungroup.morphed_tbl_graph <- function(x, ...) {
  x[] <- lapply(x, ungroup, ...)
  x
}
#' @export
dplyr::ungroup

#' @importFrom dplyr group_size
#' @export
group_size.grouped_tbl_graph <- function(x) {
  group_size(as_tibble(x))
}
#' @export
dplyr::group_size

#' @importFrom dplyr n_groups
#' @export
n_groups.grouped_tbl_graph <- function(x) {
  n_groups(as_tibble(x))
}
#' @export
dplyr::n_groups

#' @importFrom dplyr groups
#' @export
groups.grouped_tbl_graph <- function(x) {
  groups(as_tibble(x))
}
#' @export
dplyr::groups

#' @importFrom dplyr group_vars
#' @export
group_vars.grouped_tbl_graph <- function(x) {
  group_vars(as_tibble(x))
}
#' @export
dplyr::group_vars

is.grouped_tbl_graph <- function(x) {
  inherits(x, 'grouped_tbl_graph')
}
apply_groups <- function(graph, attributes) {
  attr(graph, paste0(active(graph), '_group_attr')) <- attributes
  if (!is.grouped_tbl_graph(graph)) {
    class(graph) <- c('grouped_tbl_graph', class(graph))
  }
  graph
}
