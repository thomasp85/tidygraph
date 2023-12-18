#' @importFrom dplyr group_by
#' @export
group_by.tbl_graph <- function(.data, ..., add = FALSE) {
  if (is.focused_tbl_graph(.data)) {
    cli::cli_inform('Unfocusing prior to grouping')
    .data <- unfocus(.data)
  }
  .register_graph_context(.data)
  d_tmp <- as_tibble(.data)
  d_tmp <- group_by(d_tmp, ..., .add = add)
  .data <- set_graph_data(.data, ungroup(d_tmp))
  apply_groups(.data, d_tmp)
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
  class(x) <- setdiff(class(x), 'grouped_tbl_graph')
  unfocus(x)
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
group_size.tbl_graph <- function(x) {
  group_size(as_tibble(x))
}
#' @export
dplyr::group_size

#' @importFrom dplyr n_groups
#' @export
n_groups.tbl_graph <- function(x) {
  n_groups(as_tibble(x))
}
#' @export
dplyr::n_groups

#' @importFrom dplyr groups
#' @export
groups.tbl_graph <- function(x) {
  groups(as_tibble(x))
}
#' @export
dplyr::groups

#' @importFrom dplyr group_vars
#' @export
group_vars.tbl_graph <- function(x) {
  group_vars(as_tibble(x))
}
#' @export
dplyr::group_vars

#' @importFrom dplyr group_data
#' @export
group_data.tbl_graph <- function(.data) {
  group_data(as_tibble(.data))
}
#' @export
dplyr::group_data

#' @importFrom dplyr group_indices
#' @export
group_indices.tbl_graph <- function(.data, ...) {
  group_indices(as_tibble(.data), ...)
}
#' @export
dplyr::group_indices

#' @importFrom dplyr group_keys
#' @export
group_keys.tbl_graph <- function(.tbl, ...) {
  group_keys(as_tibble(.data))
}
#' @export
dplyr::group_keys

is.grouped_tbl_graph <- function(x) {
  inherits(x, 'grouped_tbl_graph')
}
apply_groups <- function(graph, group) {
  attr(graph, paste0(active(graph), '_group_attr')) <- attributes(group)
  if (!is.grouped_tbl_graph(graph)) {
    class(graph) <- c('grouped_tbl_graph', class(graph))
  }
  graph
}
