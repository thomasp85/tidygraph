#' Create a temporary alternative representation of the graph to compute on
#'
#' The `morph`/`unmorph` verbs are used to create temporary representations of
#' the graph, such as e.g. its search tree or a subgraph. A morphed graph will
#' accept any of the standard `dplyr` verbs, and changed to the data is
#' automatically propagated to the original graph when unmorphing. Tidygraph
#' comes with a range of [morphers], but is it also possible to supply your own.
#' See Details for the requirement for custom morphers. The `crystallise` verb
#' is used to extract the temporary graph representation into a tibble
#' containing one separate graph per row and a `name` and `graph` column holding
#' the name of each graph and the graph itself respectively. `convert()` is a
#' shorthand for performing both `morph` and `crystallise` along with extracting
#' a single `tbl_graph` (defaults to the first). For morphs were you know they
#' only create a single graph, and you want to keep it, this is an easy way.
#'
#' @details
#' It is only possible to change and add to node and edge data from a
#' morphed state. Any filtering/removal of nodes and edges will not result in
#' removal from the main graph. However, nodes and edges not present in the
#' morphed state will be unaffected in the main graph when unmorphing (if new
#' columns were added during the morhped state they will be filled with `NA`).
#'
#' Morphing an already morhped graph will unmorph prior to applying the new
#' morph.
#'
#' During a morphed state, the mapping back to the original graph is stored in
#' `.tidygraph_node_index` and `.tidygraph_edge_index` columns. These are
#' accesible but protected, meaning that any changes to them with e.g. mutate
#' will be ignored. Furthermore, if the morph results in the merging of nodes
#' and/or edges the original data is stored in a `.data` column. This is
#' protected as well.
#'
#' When supplying your own morphers the morphing function should accept a
#' `tbl_graph` as its first input. The provided graph will already have nodes
#' and edges mapped with a `.tidygraph_node_index` and `.tidygraph_edge_index`
#' column. The return value must be a `tbl_graph` or a list of `tbl_graph`s and
#' these must contain either a `.tidygraph_node_index` column or a
#' `.tidygraph_edge_index` column (or both). Note that it is possible for the
#' morph to have the edges mapped back to the original nodes and vice versa
#' (e.g. as with [to_linegraph]). In that case the edge data in the morphed
#' graph(s) will contain a `.tidygraph_node_index` column and or the node data a
#' `.tidygraph_edge_index` column. If the morphing results in the collapse of
#' multiple columns or edges the index columns should be converted to list
#' columns mapping the new node/edge back to all the nodes/edges it represents.
#' Furthermore the original node/edge data should be collapsed to a list of
#' tibbles, with the row order matching the order in the index column element.
#'
#' @param .data A `tbl_graph` or a `morphed_tbl_graph`
#'
#' @param .f A morphing function. See [morphers] for a list of provided one.
#'
#' @param ... Arguments passed on to the morpher
#'
#' @param .select The graph to return during `convert()`. Either an index or the
#' name as created during `crystallise()`.
#'
#' @param .clean Should references to the node and edge indexes in the original
#' graph be removed when using `convert`
#'
#' @return A `morphed_tbl_graph`
#'
#' @export
#'
#' @examples
#' create_notable('meredith') %>%
#'   mutate(group = group_infomap()) %>%
#'   morph(to_contracted, group) %>%
#'   mutate(group_centrality = centrality_pagerank()) %>%
#'   unmorph()
morph <- function(.data, .f, ...) {
  UseMethod('morph')
}
#' @rdname morph
#' @export
unmorph <- function(.data) {
  UseMethod('unmorph')
}
#' @rdname morph
#' @export
crystallise <- function(.data) {
  UseMethod('crystallise')
}
#' @rdname morph
#' @export
crystallize <- crystallise
#' @rdname morph
#' @export
convert <- function(.data, .f, ..., .select = 1, .clean = FALSE) {
  UseMethod('convert')
}
#' @export
#' @importFrom rlang as_quosure sym quo_text enquo
morph.tbl_graph <- function(.data, .f, ...) {
  if (inherits(.data, 'grouped_tbl_graph')) {
    message('Ungrouping prior to morphing')
    .data <- ungroup(.data)
  }
  .register_graph_context(.data)
  morph_name <- quo_text(enquo(.f))
  current_active <- as_quosure(sym(active(.data)))
  .data <- mutate(activate(.data, 'nodes'), .tidygraph_node_index = seq_len(n()))
  .data <- mutate(activate(.data, 'edges'), .tidygraph_edge_index = seq_len(n()))
  .data <- activate(.data, !! current_active)
  morphed <- .f(.data, ...)
  if (!inherits(morphed, 'list')) morphed <- list(morphed)
  check_morph(morphed)
  morphed[] <- lapply(morphed, activate, what = !!current_active)
  structure(
    morphed,
    class = c('morphed_tbl_graph', 'list'),
    .orig_graph = .data,
    .morpher = morph_name
  )
}
#' @export
morph.morphed_tbl_graph <- function(.data, .f, ...) {
  message('Unmorphing tbl_graph first...')
  .data <- unmorph(.data)
  morph(.data, .f, ...)
}
#' @export
unmorph.morphed_tbl_graph <- function(.data) {
  current_active <- as_quosure(sym(active(.data[[1]])))
  nodes <- bind_rows(lapply(.data, as_tibble, active = 'nodes'))
  edges <- bind_rows(lapply(.data, as_tibble, active = 'edges'))
  graph <- attr(.data, '.orig_graph')
  real_nodes <- as_tibble(graph, active = 'nodes')
  real_edges <- as_tibble(graph, active = 'edges')
  if ('.tidygraph_node_index' %in% names(nodes)) {
    real_nodes <- merge_meta(nodes, real_nodes, '.tidygraph_node_index')
  } else if ('.tidygraph_node_index' %in% names(edges)) {
    real_nodes <- merge_meta(edges, real_nodes, '.tidygraph_node_index')
  }
  if ('.tidygraph_edge_index' %in% names(nodes)) {
    real_edges <- merge_meta(nodes, real_edges, '.tidygraph_edge_index')
  } else if ('.tidygraph_edge_index' %in% names(edges)) {
    real_edges <- merge_meta(edges, real_edges, '.tidygraph_edge_index')
  }
  real_nodes$.tidygraph_node_index <- NULL
  real_edges$.tidygraph_edge_index <- NULL
  graph <- set_node_attributes(graph, real_nodes)
  graph <- set_edge_attributes(graph, real_edges)
  activate(graph, !!current_active)
}
#' @importFrom tibble tibble
#' @importFrom rlang %||%
#' @export
crystallise.morphed_tbl_graph <- function(.data) {
  class(.data) <- 'list'
  attr(.data, '.orig_graph') <- NULL
  attr(.data, '.morpher') <- NULL
  name <- names(.data) %||% as.character(seq_along(.data))
  graph <- unname(.data)
  tibble(
    name = name,
    graph = graph
  )
}
#' @export
convert.tbl_graph <- function(.data, .f, ..., .select = 1, .clean = FALSE) {
  stopifnot(length(.select) == 1)
  graphs <- crystallise(morph(.data, .f, ...))
  if (is.character(.select)) {
    .select <- which(.select == graphs$name)[1]
    if (is.na(.select)) stop('.select does not match any named graph', call. = FALSE)
  }
  if (.select > nrow(graphs)) stop('convert did not create ', .select, ' graphs', call. = FALSE)
  graph <- graphs$graph[[.select]]
  if (.clean) {
    nodes <- as_tibble(graph, active = 'nodes')
    edges <- as_tibble(graph, active = 'edges')
    nodes$.tidygraph_node_index <- NULL
    edges$.tidygraph_edge_index <- NULL
    graph <- set_node_attributes(graph, nodes)
    graph <- set_edge_attributes(graph, edges)
  }
  graph
}
# HELPERS -----------------------------------------------------------------


#' @importFrom igraph vertex_attr_names edge_attr_names
check_morph <- function(morph) {
  if (!all(vapply(morph, inherits, logical(1), 'tbl_graph'))) {
    stop('morph must consist of tbl_graphs')
  }
  lapply(morph, function(m) {
    attr_names <- c(vertex_attr_names(m), edge_attr_names(m))
    if (!any(c('.tidygraph_node_index', '.tidygraph_edge_index') %in% attr_names)) {
      stop('Morph must contain reference to at either nodes or edges', call. = FALSE)
    }
  })
  NULL
}

merge_meta <- function(new, into, col) {
  if (is.list(new[[col]])) {
    index <- new[[col]]
    new[[col]] <- NULL
    data <- new$.orig_data
    new$.orig_data <- NULL
    new <- new[rep(seq_along(index), lengths(index)), ]
    new[[col]] <- unlist(index)
    if (!is.null(data)) {
      data <- bind_rows(data)
      data <- data[, !names(data) %in% names(new)]
      new <- bind_cols(new, data)
    }
  }
  new <- new[!is.na(new[[col]]) & !duplicated(new[[col]]), ]
  complete <- as_tibble(modifyList(
    into[new[[col]], ],
    new
  ))
  complete <- bind_rows(complete, into[!into[[col]] %in% complete[[col]], ])
  complete <- complete[order(complete[[col]]), ]
  complete
}

protect_ind <- function(data, .f, ...) {
  new_data <- .f(data, ...)
  new_data$.tidygraph_node_index <- data$.tidygraph_node_index
  new_data$.tidygraph_edge_index <- data$.tidygraph_edge_index
  if ((is.list(data$.tidygraph_node_index) || is.list(data$.tidygraph_edge_index)) && !is.null(data$.data)) {
    new_data$.data <- data$.data
  }
  new_data
}
