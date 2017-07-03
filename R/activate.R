#' Determine the context of subsequent manipulations
#'
#' As a [tbl_graph] can be considered as a collection of two linked tables it is
#' necessary to specify which table is referenced during manipulations. The
#' `activate` verb does just that and needs affects all subsequent manipulations
#' until a new table is activated. `active` is a simple query function to get
#' the currently acitve context.
#'
#' @param .data,x A tbl_graph or a grouped_tbl_graph
#'
#' @param what What should get activated? Possible values are `nodes` or
#' `edges`.
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
  message('Ungrouping graph...')
  what <- enquo(what)
  activate(ungroup(.data), !!what)
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
    stop('Only possible to activate nodes and edges', call. = FALSE)
  )
  attr(x, 'active') <- value
  x
}
