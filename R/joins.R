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
#' @importFrom stats na.omit
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

#' Join graphs on common nodes
#'
#' This graph-specific join method makes a full join on the nodes data and
#' updates the edges in the joining graph so they matches the new indexes of the
#' nodes in the resulting graph. Node and edge data is combined using
#' [dplyr::bind_rows()] semantic, meaning that data is matched by column name
#' and filled with `NA` if it is missing in either of the graphs.
#'
#' @param x A `tbl_graph`
#' @param y An object convertible to a `tbl_graph` using [as_tbl_graph()]
#' @inheritParams dplyr::full_join
#'
#' @return A `tbl_graph` containing the merged graph
#'
#' @importFrom tibble as_tibble
#' @importFrom dplyr full_join bind_rows
#' @export
#'
#' @examples
#' gr1 <- create_notable('bull') %>%
#'   activate(nodes) %>%
#'   mutate(name = letters[1:5])
#' gr2 <- create_ring(10) %>%
#'   activate(nodes) %>%
#'   mutate(name = letters[4:13])
#'
#' gr1 %>% graph_join(gr2)
graph_join <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  stopifnot(is.tbl_graph(x))
  y <- as_tbl_graph(y)

  d_tmp <- as_tibble(x, active = 'nodes')
  d_tmp2 <- as_tibble(y, active = 'nodes')
  if ('.tbl_graph_index' %in% c(names(d_tmp), names(d_tmp2))) {
    stop('The attribute name ".tbl_graph_index" is reserved', call. = FALSE)
  }
  orig_ind <- seq_len(nrow(d_tmp2))
  d_tmp2$.tbl_graph_index <- orig_ind
  nodes <- full_join(d_tmp, d_tmp2, by = by, copy = copy, suffix = suffix, ...)
  ind_lookup <- data.frame(new = seq_len(nrow(nodes)), old = nodes$.tbl_graph_index)
  nodes$.tbl_graph_index <- NULL
  edges <- as_tibble(x, active = 'edges')
  edges2 <- as_tibble(y, active = 'edges')
  edges2$from <- ind_lookup$new[match(edges2$from, ind_lookup$old)]
  edges2$to <- ind_lookup$new[match(edges2$to, ind_lookup$old)]
  edges <- bind_rows(edges, edges2)
  as_tbl_graph(list(nodes = nodes, edges = edges)) %gr_attr% y %gr_attr% x
}
