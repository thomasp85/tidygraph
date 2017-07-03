#' @export
#' @importFrom tibble as_tibble
as_tibble.tbl_graph <- function(x, active = NULL, ...) {
  if (is.null(active)) {
    active <- attr(x, 'active')
  }
  switch(
    active,
    nodes = node_tibble(x),
    edges = edge_tibble(x),
    stop('Unknown active element: ', active, '. Only nodes and edges supported', call. = FALSE)
  )
}
as_tibble.grouped_tbl_graph <- function(x, active = NULL, ...) {
  tbl <- NextMethod()
  if (is.null(active)) {
    active <- attr(x, 'active')
  }
  group_attr <- attr(x, paste0(active, '_group_attr'))
  if (!is.null(group_attr)) attributes(tbl) <- group_attr
  tbl
}
#' @export
tibble::as_tibble

#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' @importFrom igraph vertex_attr gorder
#' @importFrom tibble as_tibble
node_tibble <- function(x) {
  tbl <- as_tibble(vertex_attr(x))
  if (length(attr(tbl, 'row.names')) == 0) {
    attr(tbl, 'row.names') <- seq_len(gorder(x))
  }
  tbl
}
#' @importFrom igraph edge_attr gsize as_edgelist
#' @importFrom tibble as_tibble
#' @importFrom dplyr bind_cols
edge_tibble <- function(x) {
  tbl <- as_tibble(edge_attr(x))
  if (length(attr(tbl, 'row.names')) == 0) {
    attr(tbl, 'row.names') <- seq_len(gsize(x))
  }
  e_list <- as_edgelist(x, names = FALSE)
  mode(e_list) <- 'integer'
  e_list <- as_tibble(e_list)
  names(e_list) <- c('from', 'to')
  bind_cols(e_list, tbl)
}
