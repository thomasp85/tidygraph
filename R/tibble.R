#' @export
#' @importFrom tibble as_tibble
as_tibble.tbl_graph <- function(x, active = NULL, focused = TRUE, ...) {
  if (is.null(active)) {
    active <- attr(x, 'active')
  }
  switch(
    active,
    nodes = node_tibble(x, focused = focused),
    edges = edge_tibble(x, focused = focused),
    cli::cli_abort('Unknown active element: {.val {active}}. Only nodes and edges supported')
  )
}
#' @export
as_tibble.grouped_tbl_graph <- function(x, active = NULL, focused = TRUE, ...) {
  tbl <- NextMethod()
  if (is.null(active)) {
    active <- attr(x, 'active')
  }
  group_attr <- attr(x, paste0(active, '_group_attr'))
  if (!is.null(group_attr)) attributes(tbl) <- group_attr
  tbl
}
#' @export
as_tibble.morphed_tbl_graph <- function(x, ...) {
  as_tibble(crystallize(x), ...)
}
#' @export
tibble::as_tibble

#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' @importFrom igraph vertex_attr gorder
#' @importFrom tibble as_tibble
node_tibble <- function(x, focused = TRUE) {
  tbl <- as_tibble(vertex_attr(x))
  if (length(attr(tbl, 'row.names')) == 0) {
    attr(tbl, 'row.names') <- .set_row_names(gorder(x))
  }
  if (focused && is.focused_tbl_graph(x)) {
    tbl <- tbl[focus_ind(x, 'nodes'), ]
  }
  tbl
}
#' @importFrom igraph edge_attr gsize as_edgelist
#' @importFrom tibble as_tibble
#' @importFrom dplyr bind_cols
edge_tibble <- function(x, focused = TRUE) {
  tbl <- as_tibble(edge_attr(x))
  if (length(attr(tbl, 'row.names')) == 0) {
    attr(tbl, 'row.names') <- .set_row_names(gsize(x))
  }
  e_list <- as_edgelist(x, names = FALSE)
  mode(e_list) <- 'integer'
  colnames(e_list) <- c('from', 'to')
  e_list <- as_tibble(e_list)
  tbl <- bind_cols(e_list, tbl)
  if (focused && is.focused_tbl_graph(x)) {
    tbl <- tbl[focus_ind(x, 'edges'), ]
  }
  tbl
}
