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
#' @export
.G <- function() {
  .graph_context$graph()
}
#' @export
.N <- function() {
  .graph_context$nodes()
}
#' @export
.E <- function() {
  .graph_context$edges()
}
