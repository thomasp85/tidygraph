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
#' @param value 
#' @return A tbl_graph
#'
#' @note Activate will ungroup a grouped_tbl_graph.
#'
#' @export
#' @importFrom activate activate active active<-
#' @name activate
#' @export activate active 
#' @examples
#' gr <- as_tbl_graph(matrix(sample(5, 16, TRUE), ncol = 2))
#' gr <- gr %>%
#'   activate(nodes) %>%
#'   mutate(class = sample(c('a', 'b'), 5, TRUE)) %>%
#'   activate(edges) %>%
#'   arrange(from)
#'
#' # activate will do standard evaluation if the provided symbol is anything
#' # other than nodes and edges
#' context <- 'nodes'
#' gr %>%
#'   activate(context) %>%
#'   as_tibble()
activate.tbl_graph <- function(.data, what) {
  what_name <- deparse(substitute(what))
  if (what_name %in% c('nodes', 'edges')) what <- what_name
  active(.data) <- what
  .data
}
#' @rdname activate
#' @export
activate.grouped_tbl_graph <- function(.data, what) {
  message('Ungrouping graph...')
  activate::activate(ungroup(.data), what)
}

#' @name activate
#' @rdname activate
#' @export
active.tbl_graph <- function(x) {
  attr(x, 'active')
}
#' @name active
#' @rdname activate
#' @keywords internal
`active<-.tbl_graph` <- function(x, value) {
  if (!value %in% c('nodes', 'edges')) {
    stop('Only possible to activate nodes and edges', call. = FALSE)
  }
  attr(x, 'active') <- value
  x
}
