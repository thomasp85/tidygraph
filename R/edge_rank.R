#' Calculate edge ranking
#'
#' This set of functions tries to calculate a ranking of the edges in a graph so
#' that edges sharing certain topological traits are in proximity in the
#' resulting order.
#'
#' @param cyclic should the eulerian path start and end at the same node
#'
#' @return An integer vector giving the position of each edge in the ranking
#'
#' @rdname edge_rank
#' @name edge_rank
#'
#' @examples
#' graph <- create_notable('meredith') %>%
#'   activate(edges) %>%
#'   mutate(rank = edge_rank_eulerian())
#'
NULL

#' @describeIn edge_rank Calculcate ranking as the visit order of a eulerian
#' path or cycle. If no such path or cycle exist it will return a vector of
#' `NA`s
#' @importFrom igraph eulerian_path eulerian_cycle
#' @export
edge_rank_eulerian <- function(cyclic = FALSE) {
  expect_edges()
  alg <- if (cyclic) eulerian_cycle else eulerian_path
  rlang::try_fetch({
      compress_rank(match(focus_ind(.G(), 'edges'), alg(.G())$epath))
    },
    error = function(...) {
      rep_len(NA_integer_, length(focus_ind(.G(), 'edges')))
    }
  )
}
