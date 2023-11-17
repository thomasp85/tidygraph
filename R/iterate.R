#' Repeatedly modify a graph by a function
#'
#' The iterate family of functions allow you to call the same modification
#' function on a graph until some condition is met. This can be used to create
#' simple simulations in a tidygraph friendly API
#'
#' @param .data A `tbl_graph` object
#' @param .f A function taking in a `tbl_graph` as the first argument and
#' returning a `tbl_graph` object
#' @param n The number of times to iterate
#' @param cnd A condition that must evaluate to `TRUE` or `FALSE` determining if
#' iteration should continue
#' @param max_n The maximum number of iterations in case `cnd` never evaluates
#' to `FALSE`
#' @param ... Further arguments passed on to `.f`
#'
#' @return A `tbl_graph` object
#'
#' @rdname iterate
#' @name iterate
#'
#' @examples
#' # Gradually remove edges from the least connected nodes while avoiding
#' # isolates
#' create_notable('zachary') |>
#'   iterate_n(20, function(gr) {
#'     gr |>
#'       activate(nodes) |>
#'       mutate(deg = centrality_degree(), rank = order(deg)) |>
#'       activate(edges) |>
#'       slice(
#'         -which(edge_is_incident(.N()$rank == sum(.N()$deg == 1) + 1))[1]
#'       )
#'   })
#'
#' # Remove a random edge until the graph is split in two
#' create_notable('zachary') |>
#'   iterate_while(graph_component_count() == 1, function(gr) {
#'     gr |>
#'       activate(edges) |>
#'       slice(-sample(graph_size(), 1))
#'   })
#'
NULL

#' @rdname iterate
#' @export
#'
iterate_n <- function(.data, n, .f, ...) {
  check_tbl_graph(.data)
  .f <- rlang::as_function(.f)
  act <- active(.data)
  for (i in seq_len(n)) {
    .data <- .f(.data, ...)
    check_tbl_graph(.data)
  }
  activate(.data, !!rlang::sym(act))
}

#' @rdname iterate
#' @export
#'
iterate_while <- function(.data, cnd, .f, ..., max_n = NULL) {
  check_tbl_graph(.data)
  .f <- rlang::as_function(.f)
  act <- active(.data)
  if (!is.null(max_n) && !rlang::is_integerish(max_n, 1, TRUE)) {
    cli::cli_abort('{.arg max_n} must either be NULL or a single integer')
  }
  cnd <- rlang::enquo(cnd)
  cnd <- rlang::expr(with_graph(.data, !!cnd))
  n <- 1
  while (isTRUE(rlang::eval_tidy(cnd)) && !isTRUE(n > max_n)) {
    .data <- .f(.data, ...)
    check_tbl_graph(.data)
    n <- n + 1
  }
  activate(.data, !!rlang::sym(act))
}
