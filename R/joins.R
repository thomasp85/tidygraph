#' @importFrom dplyr left_join
#' @export
left_join.tbl_graph <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  d_tmp <- as_tibble(x)
  d_tmp <- left_join(d_tmp, y, by = by, copy = copy, suffix = suffix, ...)
  set_graph_data(x, d_tmp)
}
#' @export
dplyr::left_join

#' @importFrom dplyr right_join
#' @export
right_join.tbl_graph <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  d_tmp <- as_tibble(x)
  if ('.tbl_graph_index' %in% names(d_tmp)) {
    stop('The attribute name ".tbl_graph_index" is reserved', call. = FALSE)
  }
  if (active(x) == 'edges' && (!all(c('from', 'to') %in% names(y)) ||
                               !(is.numeric(y$from) && is.numeric(y$to)))) {
    stop('y must contain the numeric columns "from" and "to"', call. = FALSE)
  }
  orig_ind <- seq_len(nrow(d_tmp))
  d_tmp$.tbl_graph_index <- orig_ind
  d_tmp <- right_join(d_tmp, y, by = by, copy = copy, suffix = suffix, ...)
  new_order <- order(d_tmp$.tbl_graph_index) # Will never eclipse Joy Division
  d_tmp <- d_tmp[new_order, ]
  x <- slice(x, na.omit(d_tmp$.tbl_graph_index))
  new_rows <- which(is.na(d_tmp$.tbl_graph_index))
  d_tmp$.tbl_graph_index <- NULL
  x <- switch(
    active(x),
    nodes = add_vertices(x, length(new_rows)),
    edges = add_edges(x, as.vector(rbind(d_tmp$from[new_rows], d_tmp$to[new_rows])))
  ) %gr_attr% x
  x <- set_graph_data(x, d_tmp)
  arrange(x, seq_len(nrow(d_tmp))[new_order])
}
#' @export
dplyr::right_join

#' @importFrom dplyr inner_join
#' @export
inner_join.tbl_graph <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  d_tmp <- as_tibble(x)
  if ('.tbl_graph_index' %in% names(d_tmp)) {
    stop('The attribute name ".tbl_graph_index" is reserved', call. = FALSE)
  }
  orig_ind <- seq_len(nrow(d_tmp))
  d_tmp$.tbl_graph_index <- orig_ind
  d_tmp <- inner_join(d_tmp, y, by = by, copy = copy, suffix = suffix, ...)
  x <- slice(x, d_tmp$.tbl_graph_index)
  d_tmp$.tbl_graph_index <- NULL
  set_graph_data(x, d_tmp)
}
#' @export
dplyr::inner_join

#' @importFrom dplyr full_join
#' @importFrom igraph add_vertices add_edges
#' @export
full_join.tbl_graph <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  d_tmp <- as_tibble(x)
  if ('.tbl_graph_index' %in% names(d_tmp)) {
    stop('The attribute name ".tbl_graph_index" is reserved', call. = FALSE)
  }
  if (active(x) == 'edges' && (!all(c('from', 'to') %in% names(y)) ||
                               !(is.numeric(y$from) && is.numeric(y$to)))) {
    stop('y must contain the numeric columns "from" and "to"', call. = FALSE)
  }
  orig_ind <- seq_len(nrow(d_tmp))
  d_tmp$.tbl_graph_index <- orig_ind
  d_tmp <- full_join(d_tmp, y, by = by, copy = copy, suffix = suffix, ...)
  new_rows <- which(is.na(d_tmp$.tbl_graph_index))
  d_tmp$.tbl_graph_index <- NULL
  x <- switch(
    active(x),
    nodes = add_vertices(x, length(new_rows)),
    edges = add_edges(x, as.vector(rbind(d_tmp$from[new_rows], d_tmp$to[new_rows])))
  ) %gr_attr% x
  set_graph_data(x, d_tmp)
}
#' @export
dplyr::full_join

#' @importFrom dplyr semi_join
#' @export
semi_join.tbl_graph <- function(x, y, by = NULL, copy = FALSE, ...) {
  d_tmp <- as_tibble(x)
  if ('.tbl_graph_index' %in% names(d_tmp)) {
    stop('The attribute name ".tbl_graph_index" is reserved', call. = FALSE)
  }
  orig_ind <- seq_len(nrow(d_tmp))
  d_tmp$.tbl_graph_index <- orig_ind
  d_tmp <- semi_join(d_tmp, y, by = by, copy = copy, ...)
  slice(x, d_tmp$.tbl_graph_index)
}
#' @export
dplyr::semi_join

#' @importFrom dplyr anti_join
#' @export
anti_join.tbl_graph <- function(x, y, by = NULL, copy = FALSE, ...) {
  d_tmp <- as_tibble(x)
  if ('.tbl_graph_index' %in% names(d_tmp)) {
    stop('The attribute name ".tbl_graph_index" is reserved', call. = FALSE)
  }
  orig_ind <- seq_len(nrow(d_tmp))
  d_tmp$.tbl_graph_index <- orig_ind
  d_tmp <- anti_join(d_tmp, y, by = by, copy = copy, ...)
  slice(x, d_tmp$.tbl_graph_index)
}
#' @export
dplyr::anti_join
