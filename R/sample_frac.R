#' @export
#' @importFrom dplyr sample_frac
#' @importFrom igraph delete_vertices delete_edges
#' @importFrom rlang enquo
sample_frac.tbl_graph <- function(tbl, size = 1, replace = FALSE, weight = NULL, .env = parent.frame(), ...) {
  d_tmp <- as_tibble(tbl)
  weight <- enquo(weight)
  if ('.tbl_graph_index' %in% names(d_tmp)) {
    cli::cli_abort('The attribute name {.field .tbl_graph_index} is reserved')
  }
  orig_ind <- seq_len(nrow(d_tmp))
  d_tmp$.tbl_graph_index <- orig_ind
  d_tmp <- sample_frac(d_tmp, size = size, replace = replace, weight = !! weight, .env = .env)
  remove_ind <- orig_ind[-d_tmp$.tbl_graph_index]
  switch(
    active(tbl),
    nodes = delete_vertices(tbl, remove_ind),
    edges = delete_edges(tbl, remove_ind)
  ) %gr_attr% tbl
}
#' @export
#' @importFrom dplyr sample_frac
#' @importFrom rlang enquo
sample_frac.morphed_tbl_graph <- function(tbl, size = 1, replace = FALSE, weight = NULL, .env = parent.frame(), ...) {
  weight <- enquo(weight)
  tbl[] <- lapply(tbl, sample_frac, size = size, replace = replace, weight = !! weight, .env = .env)
  tbl
}

#' @export
dplyr::sample_frac
