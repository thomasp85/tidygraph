#' @importFrom R6 R6Class
ContextBuilder <- R6Class(
  'ContextBuilder',
  public = list(
    set = function(graph) {
      stopifnot(inherits(graph, 'tbl_graph'))
      private$context <- c(private$context, list(graph))
      invisible(self)
    },
    clear = function() {
      private$context <- private$context[-length(private$context)]
    },
    alive = function() {
      length(private$context) != 0
    },
    graph = function() {
      private$check()
      private$context[[length(private$context)]]
    },
    nodes = function() {
      as_tibble(self$graph(), active = 'nodes')
    },
    edges = function() {
      as_tibble(self$graph(), active = 'edges')
    },
    active = function() {
      private$check()
      active(self$graph())
    }
  ),
  private = list(
    context = list(),
    check = function() {
      if (!self$alive()) {
        stop('This function should not be called directly', call. = FALSE)
      }
    }
  )
)
.graph_context <- ContextBuilder$new()
expect_nodes <- function() {
  if (.graph_context$active() != 'nodes') {
    stop('This call requires nodes to be active', call. = FALSE)
  }
}
expect_edges <- function() {
  if (.graph_context$active() != 'edges') {
    stop('This call requires edges to be active', call. = FALSE)
  }
}

#' Access graph, nodes, and edges directly inside verbs
#'
#' These three functions makes it possible to directly access either the node
#' data, the edge data or the graph itself while computing inside verbs. It is
#' e.g. possible to add an attribute from the node data to the edges based on
#' the terminating nodes of the edge, or extract some statistics from the graph
#' itself to use in computations.
#'
#' @return Either a `tbl_graph` (`.G()`) or a `tibble` (`.N()`)
#'
#' @rdname context_accessors
#' @name context_accessors
#'
#' @examples
#'
#' # Get data from the nodes while computing for the edges
#' create_notable('bull') %>%
#'   activate(nodes) %>%
#'   mutate(centrality = centrality_power()) %>%
#'   activate(edges) %>%
#'   mutate(mean_centrality = (.N()$centrality[from] + .N()$centrality[to])/2)
NULL

#' @describeIn context_accessors Get the tbl_graph you're currently working on
#' @export
.G <- function() {
  .graph_context$graph()
}
#' @describeIn context_accessors Get the nodes data from the graph you're currently working on
#' @export
.N <- function() {
  .graph_context$nodes()
}
#' @describeIn context_accessors Get the edges data from the graph you're currently working on
#' @export
.E <- function() {
  .graph_context$edges()
}
