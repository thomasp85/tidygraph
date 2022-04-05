#' Determine the context of subsequent manipulations
#'
#' As a [tbl_graph] can be considered as a collection of two linked tables it is
#' necessary to specify which table is referenced during manipulations. The
#' `activate` verb does just that and needs affects all subsequent manipulations
#' until a new table is activated. `active` is a simple query function to get
#' the currently acitve context. In addition to the use of `activate` it is also
#' possible to activate nodes or edges as part of the piping using the `%N>%`
#' and `%E>%` pipes respectively. Do note that this approach somewhat obscures
#' what is going on and is thus only recommended for quick, one-line, fixes in
#' interactive use.
#'
#' @param .data,x,lhs A tbl_graph or a grouped_tbl_graph
#'
#' @param what What should get activated? Possible values are `nodes` or
#' `edges`.
#'
#' @param rhs A function to pipe into
#'
#' @return A tbl_graph
#'
#' @note Activate will ungroup a grouped_tbl_graph.
#'
#' @export
#'
#' @examples
#' gr <- create_complete(5) %>%
#'   activate(nodes) %>%
#'   mutate(class = sample(c('a', 'b'), 5, TRUE)) %>%
#'   activate(edges) %>%
#'   arrange(from)
#'
#' # The above could be achieved using the special pipes as well
#' gr <- create_complete(5) %N>%
#'   mutate(class = sample(c('a', 'b'), 5, TRUE)) %E>%
#'   arrange(from)
#' # But as you can see it obscures what part of the graph is being targeted
#'
activate <- function(.data, what) {
  UseMethod('activate')
}
#' @export
#' @importFrom rlang enquo quo_text
activate.tbl_graph <- function(.data, what) {
  active(.data) <- quo_text(enquo(what))
  .data
}
#' @export
#' @importFrom rlang enquo
activate.grouped_tbl_graph <- function(.data, what) {
  what <- enquo(what)
#  if (gsub('"', '', quo_text(what)) == active(.data)) {
#    return(.data)
#  }
  cli::cli_inform('Ungrouping {.arg .data}...')
  .data <- ungroup(.data)
  activate(.data, !!what)
}
#' @export
activate.morphed_tbl_graph <- function(.data, what) {
  what <- enquo(what)
  .data[] <- lapply(.data, activate, what = !!what)
  .data
}

#' @rdname activate
#' @export
active <- function(x) {
  attr(x, 'active')
}
`active<-` <- function(x, value) {
  value <- gsub('"', '', value)
  value <- switch(
    value,
    vertices = ,
    nodes = 'nodes',
    links = ,
    edges = 'edges',
    cli::cli_abort('Only possible to activate nodes and edges')
  )
  attr(x, 'active') <- value
  x
}

#' @rdname activate
#' @importFrom rlang enexpr eval_bare caller_env
#' @importFrom magrittr %>%
#' @export
`%N>%` <- function(lhs, rhs) {
  rhs <- enexpr(rhs)
  lhs <- activate(lhs, 'nodes')

  # Magrittr does not support inlining so caller
  # _must_ have `%>%` in scope
  expr <- call('%>%', lhs, rhs)
  eval_bare(expr, caller_env())
}
#' @rdname activate
#' @importFrom rlang enexpr eval_bare caller_env
#' @importFrom magrittr %>%
#' @export
`%E>%` <- function(lhs, rhs) {
  rhs <- enexpr(rhs)
  lhs <- activate(lhs, 'edges')

  # Magrittr does not support inlining so caller
  # _must_ have `%>%` in scope
  expr <- call('%>%', lhs, rhs)
  eval_bare(expr, caller_env())
}
