#' @export
#' @importFrom dplyr slice
slice.tbl_graph <- function(.data, ..., .by = NULL, .preserve = FALSE) {
  .data <- unfocus(.data)
  graph_slicer(.data, slice, ..., .by = {{.by}}, .preserve = .preserve)
}
#' @export
#' @importFrom dplyr slice
slice.morphed_tbl_graph <- function(.data, ..., .by = NULL, .preserve = FALSE) {
  .data[] <- lapply(.data, function(d) slice(d, ..., .by = {{.by}}, .preserve = .preserve))
  .data
}
#' @export
dplyr::slice

#' @export
#' @importFrom dplyr slice_head
slice_head.tbl_graph <- function(.data, ..., n, prop, by = NULL) {
  .data <- unfocus(.data)
  graph_slicer(.data, slice_head, ..., n = n, prop = prop, by = {{by}})
}
#' @export
#' @importFrom dplyr slice_head
slice_head.morphed_tbl_graph <- function(.data, ..., n, prop, by = NULL) {
  .data[] <- lapply(.data, function(d) slice_head(d, ..., n = n, prop = prop, by = {{by}}))
  .data
}
#' @export
dplyr::slice_head

#' @export
#' @importFrom dplyr slice_tail
slice_tail.tbl_graph <- function(.data, ..., n, prop, by = NULL) {
  .data <- unfocus(.data)
  graph_slicer(.data, slice_tail, ..., n = n, prop = prop, by = {{by}})
}
#' @export
#' @importFrom dplyr slice_tail
slice_tail.morphed_tbl_graph <- function(.data, ..., n, prop, by = NULL) {
  .data[] <- lapply(.data, function(d) slice_tail(d, ..., n = n, prop = prop, by = {{by}}))
  .data
}
#' @export
dplyr::slice_tail

#' @export
#' @importFrom dplyr slice_min
slice_min.tbl_graph <- function(.data, order_by, ..., n, prop, by = NULL, with_ties = TRUE, na_rm = FALSE) {
  .data <- unfocus(.data)
  graph_slicer(.data, slice_min, order_by = {{order_by}}, ..., n = n, prop = prop, by = {{by}}, with_ties = with_ties, na_rm = na_rm)
}
#' @export
#' @importFrom dplyr slice_min
slice_min.morphed_tbl_graph <- function(.data, order_by, ..., n, prop, by = NULL, with_ties = TRUE, na_rm = FALSE) {
  .data[] <- lapply(.data, function(d) slice_min(d, order_by = {{order_by}}, ..., n = n, prop = prop, by = {{by}}, with_ties = with_ties, na_rm = na_rm))
  .data
}
#' @export
dplyr::slice_min

#' @export
#' @importFrom dplyr slice_max
slice_max.tbl_graph <- function(.data, order_by, ..., n, prop, by = NULL, with_ties = TRUE, na_rm = FALSE) {
  .data <- unfocus(.data)
  graph_slicer(.data, slice_max, order_by = {{order_by}}, ..., n = n, prop = prop, by = {{by}}, with_ties = with_ties, na_rm = na_rm)
}
#' @export
#' @importFrom dplyr slice_max
slice_max.morphed_tbl_graph <- function(.data, order_by, ..., n, prop, by = NULL, with_ties = TRUE, na_rm = FALSE) {
  .data[] <- lapply(.data, function(d) slice_max(d, order_by = {{order_by}}, ..., n = n, prop = prop, by = {{by}}, with_ties = with_ties, na_rm = na_rm))
  .data
}
#' @export
dplyr::slice_max

#' @export
#' @importFrom dplyr slice_sample
slice_sample.tbl_graph <- function(.data, ..., n, prop, by = NULL, weight_by = NULL, replace = FALSE) {
  .data <- unfocus(.data)
  graph_slicer(.data, slice_sample, ..., n = n, prop = prop, by = {{by}}, weight_by = weight_by, replace = replace)
}
#' @export
#' @importFrom dplyr slice_sample
slice_sample.morphed_tbl_graph <- function(.data, ..., n, prop, by = NULL, weight_by = NULL, replace = FALSE) {
  .data[] <- lapply(.data, function(d) slice_sample(d, ..., n = n, prop = prop, by = {{by}}, weight_by = weight_by, replace = replace))
  .data
}
#' @export
dplyr::slice_sample

#' @importFrom igraph delete_vertices delete_edges
graph_slicer <- function(.data, slicer, ...) {
  .register_graph_context(.data)
  d_tmp <- as_tibble(.data)
  check_reserved(d_tmp)
  orig_ind <- seq_len(nrow(d_tmp))
  d_tmp$.tbl_graph_index <- orig_ind
  d_tmp <- slicer(d_tmp, ...)
  remove_ind <- if (nrow(d_tmp) == 0) orig_ind else orig_ind[-d_tmp$.tbl_graph_index]
  switch(
    active(.data),
    nodes = delete_vertices(.data, remove_ind),
    edges = delete_edges(.data, remove_ind)
  ) %gr_attr% .data
}

