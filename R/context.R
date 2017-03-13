#' @importFrom R6 R6Class
ContextBuilder <- R6Class(
  'ContextBuilder',
  public = list(
    set = function(graph) {
      stopifnot(inherits(graph, 'tbl_graph'))
      private$context <- graph
      invisible(self)
    },
    clear = function() {
      private$context <- NULL
    },
    alive = function() {
      !is.null(private$context)
    },
    graph = function() {
      private$check()
      private$context
    },
    nodes = function() {
      as_tibble(self$graph, active = 'nodes')
    },
    edges = function() {
      as_tibble(self$graph, active = 'edges')
    }
  ),
  private = list(
    context = NULL,
    check = function() {
      if (!self$alive()) {
        stop('This function should not be called directly', call. = FALSE)
      }
    }
  )
)
.graph_context <- ContextBuilder$new()

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
