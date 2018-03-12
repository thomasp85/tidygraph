#' @importFrom rlang quos !!!
#' @export
mutate.tbl_graph <- function(.data, ...) {
  dots <- quos(...)
  for (i in seq_along(dots)) {
    dot <- dots[i]
    .data <- mutate_as_tbl(.data, !!!dot)
  }
  .data
}
#' Base implementation of mutate
#'
#' This implementation of mutate is slightly faster than `mutate` at the expense
#' of the graph only being updated in the end. This means that graph algorithms
#' will not take changes happening during the mutate call into account.
#'
#' @details
#' The order of speed increase are rather small and in the ~1 millisecond per
#' mutateed column order, so for regular use this should not be a choice. The
#' operations not supported by `mutate_as_tbl` are e.g.
#'
#' ```
#' gr %>%
#'   activate(nodes) %>%
#'   mutate(weights = runif(10), degree = centrality_degree(weights))
#' ```
#'
#' as `weights` will only be made available in the graph at the end of the
#' mutate call.
#'
#' @param .data A `tbl_graph` object
#'
#' @param ... columns to mutate
#'
#' @return A `tbl_graph` object
#'
#' @keywords internal
#' @importFrom dplyr mutate
#' @export
mutate_as_tbl <- function(.data, ...) {
  .register_graph_context(.data)
  d_tmp <- as_tibble(.data)
  d_tmp <- mutate(d_tmp, ...)
  set_graph_data(.data, d_tmp)
}
#' @export
#' @importFrom dplyr mutate
mutate.morphed_tbl_graph <- function(.data, ...) {
  .data[] <- lapply(.data, protect_ind, .f = mutate, ...)
  .data
}
#' @export
dplyr::mutate

#' @importFrom dplyr transmute
#' @export
dplyr::transmute

#' @importFrom dplyr mutate_all
#' @export
dplyr::mutate_all

#' @importFrom dplyr mutate_at
#' @export
dplyr::mutate_at

#' @importFrom dplyr n
#' @export
dplyr::n
