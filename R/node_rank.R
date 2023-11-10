#' Calculate node ranking
#'
#' This set of functions tries to calculate a ranking of the nodes in a graph so
#' that nodes sharing certain topological traits are in proximity in the
#' resulting order. These functions are of great value when composing matrix
#' layouts and arc diagrams but could concievably be used for other things as
#' well.
#'
#' @param dist The algorithm to use for deriving a distance matrix from the
#' graph. One of
#'
#' - `"shortest"` (default): Use the shortest path between all nodes
#' - `"euclidean"`: Calculate the L2 norm on the adjacency matrix of the graph
#' - `"manhattan"`: Calculate the L1 norm on the adjacency matrix of the graph
#' - `"maximum"`: Calculate the supremum norm on the adjacenecy matrix of the graph
#' - `"canberra"`: Calculate a weighted manhattan distance on the adjacency matrix of the graph
#' - `"binary"`: Calculate distance as the proportion of agreement between nodes based on the adjacency matrix of the graph
#'
#' or a function that takes a `tbl_graph` and return a `dist` object with a size
#' matching the order of the graph.
#'
#' @param mode Which edges should be included in the distance calculation. For
#' distance measures based on the adjacency matrix, `'out' ` will use the matrix
#' as is, `'in'` will use the transpose, and `'all'` will take the mean of the
#' two. Defaults to `'out'`. Ignored for undirected graphs.
#'
#' @param weights An edge variable to use as weight for the shortest path
#' calculation if `dist = 'shortest'`
#'
#' @param algorithm The algorithm to use for the shortest path calculation if
#' `dist = 'shortest'`
#'
#' @param ... Arguments passed on to other algorithms. See *Functions* section for reference
#'
#' @return An integer vector giving the position of each node in the ranking
#'
#' @rdname node_rank
#' @name node_rank
#'
#' @examples
#' graph <- create_notable('zachary') %>%
#'   mutate(rank = node_rank_hclust())
#'
NULL

#' @describeIn node_rank Use hierarchical clustering to rank nodes (see [stats::hclust()] for allowed methods)
#' @param method The method to use. See *Functions* section for reference
#' @importFrom rlang enquo eval_tidy
#' @importFrom stats hclust
#' @export
node_rank_hclust <- function(method = 'average', dist = 'shortest', mode = 'out', weights = NULL, algorithm = 'automatic') {
  weights <- enquo(weights)
  weights <- eval_tidy(weights, .E())
  if (is.null(weights)) {
    weights <- NA
  }
  mat <- to_dist(.G(), dist, mode, weights, algorithm)
  order(hclust(mat, method)$order)
}
#' @describeIn node_rank Use simulated annealing based on the "ARSA" method in `seriation`
#' @param cool cooling rate
#' @param tmin minimum temperature
#' @param swap_to_inversion Proportion of swaps in local neighborhood search
#' @param step_multiplier Multiplication factor for number of iterations per temperature
#' @param reps Number of repeats with random initialisation
#' @importFrom rlang enquo eval_tidy
#' @export
node_rank_anneal <- function(cool = 0.5, tmin = 1e-4, swap_to_inversion = 0.5, step_multiplier = 100, reps = 1, dist = 'shortest', mode = 'out', weights = NULL, algorithm = 'automatic') {
  expect_nodes()
  weights <- enquo(weights)
  weights <- eval_tidy(weights, .E())
  if (is.null(weights)) {
    weights <- NA
  }
  mat <- to_dist(.G(), dist, mode, weights, algorithm)
  control <- list(cool = cool, tmin = tmin, swap_to_inversion = swap_to_inversion, try_multiplier = step_multiplier, reps = reps)
  seriate(mat, 'ARSA', control)
}
#' @describeIn node_rank Use branch and bounds strategy to minimize the gradient measure (only feasable for small graphs). Will use "BBURCG" or "BBWRCG" in `seriation` dependent on the `weighted_gradient` argument
#' @param weighted_gradient minimize the weighted gradient measure? Defaults to `FALSE`
#' @importFrom rlang enquo eval_tidy
#' @export
node_rank_branch_bound <- function(weighted_gradient = FALSE, dist = 'shortest', mode = 'out', weights = NULL, algorithm = 'automatic') {
  expect_nodes()
  weights <- enquo(weights)
  weights <- eval_tidy(weights, .E())
  if (is.null(weights)) {
    weights <- NA
  }
  mat <- to_dist(.G(), dist, mode, weights, algorithm)
  method <- if (weighted_gradient) 'BBWRCG' else "BBURCG"
  seriate(mat, method, list())
}
#' @describeIn node_rank Minimize hamiltonian path length using a travelling salesperson solver. See the the `solve_TSP` function in `TSP` for an overview of possible arguments
#' @importFrom rlang enquo eval_tidy
#' @export
node_rank_traveller <- function(method = 'two_opt', ..., dist = 'shortest', mode = 'out', weights = NULL, algorithm = 'automatic') {
  expect_nodes()
  weights <- enquo(weights)
  weights <- eval_tidy(weights, .E())
  if (is.null(weights)) {
    weights <- NA
  }
  mat <- to_dist(.G(), dist, mode, weights, algorithm)
  control <- list(method = method, ...)
  seriate(mat, 'TSP', control)
}
#' @describeIn node_rank Use Rank-two ellipse seriation to rank the nodes. Uses "R2E" method in `seriation`
#' @importFrom rlang enquo eval_tidy
#' @export
node_rank_two <- function(dist = 'shortest', mode = 'out', weights = NULL, algorithm = 'automatic') {
  expect_nodes()
  weights <- enquo(weights)
  weights <- eval_tidy(weights, .E())
  if (is.null(weights)) {
    weights <- NA
  }
  mat <- to_dist(.G(), dist, mode, weights, algorithm)
  seriate(mat, 'R2E', list())
}
#' @describeIn node_rank Rank by multidimensional scaling onto one dimension. `method = 'cmdscale'` will use the classic scaling from `stats`, `method = 'isoMDS'` will use `isoMDS` from `MASS`, and `method = 'sammon'` will use `sammon` from `MASS`
#' @importFrom rlang enquo eval_tidy
#' @export
node_rank_mds <- function(method = 'cmdscale', dist = 'shortest', mode = 'out', weights = NULL, algorithm = 'automatic') {
  expect_nodes()
  weights <- enquo(weights)
  weights <- eval_tidy(weights, .E())
  if (is.null(weights)) {
    weights <- NA
  }
  mat <- to_dist(.G(), dist, mode, weights, algorithm)
  seriate(mat, 'MDS', list(method = method))
}
#' @describeIn node_rank Minimize hamiltonian path length by reordering leafs in a hierarchical clustering. Method refers to the clustering algorithm (either 'average', 'single', 'complete', or 'ward')
#' @param type The type of leaf reordering, either `'GW'` to use the "GW" method or `'OLO'` to use the "OLO" method (both in `seriation`)
#' @importFrom rlang enquo eval_tidy
#' @export
node_rank_leafsort <- function(method = 'average', type = 'OLO', dist = 'shortest', mode = 'out', weights = NULL, algorithm = 'automatic') {
  expect_nodes()
  weights <- enquo(weights)
  weights <- eval_tidy(weights, .E())
  if (is.null(weights)) {
    weights <- NA
  }
  mat <- to_dist(.G(), dist, mode, weights, algorithm)
  seriate(mat, type, list(method = method))
}
#' @describeIn node_rank Use Prim's algorithm to find a minimum spanning tree giving the rank. Uses the "VAT" method in `seriation`
#' @importFrom rlang enquo eval_tidy
#' @export
node_rank_visual <- function(dist = 'shortest', mode = 'out', weights = NULL, algorithm = 'automatic') {
  expect_nodes()
  weights <- enquo(weights)
  weights <- eval_tidy(weights, .E())
  if (is.null(weights)) {
    weights <- NA
  }
  mat <- to_dist(.G(), dist, mode, weights, algorithm)
  seriate(mat, 'VAT', list())
}
#' @describeIn node_rank Minimize the 2-sum problem using a relaxation approach. Uses the "Spectral" or "Spectral_norm" methods in `seriation` depending on the value of the `norm` argument
#' @param normalized Should the normalized laplacian of the similarity matrix be used?
#' @importFrom rlang enquo eval_tidy
#' @export
node_rank_spectral <- function(normalized = FALSE, dist = 'shortest', mode = 'out', weights = NULL, algorithm = 'automatic') {
  expect_nodes()
  weights <- enquo(weights)
  weights <- eval_tidy(weights, .E())
  if (is.null(weights)) {
    weights <- NA
  }
  mat <- to_dist(.G(), dist, mode, weights, algorithm)
  method <- if(normalized) 'Spectral_norm' else 'Spectral'
  seriate(mat, method, list())
}
#' @describeIn node_rank Sorts points into neighborhoods by pushing large distances away from the diagonal. Uses the "SPIN_STS" method in `seriation`
#' @param step The number iterations to run per initialisation
#' @param nstart The number of random initialisations to perform
#' @importFrom rlang enquo eval_tidy
#' @export
node_rank_spin_out <- function(step = 25, nstart = 10, dist = 'shortest', mode = 'out', weights = NULL, algorithm = 'automatic') {
  expect_nodes()
  weights <- enquo(weights)
  weights <- eval_tidy(weights, .E())
  if (is.null(weights)) {
    weights <- NA
  }
  mat <- to_dist(.G(), dist, mode, weights, algorithm)
  seriate(mat, 'SPIN_STS', list(step = step, nstart = nstart))
}
#' @describeIn node_rank Sorts points into neighborhoods by concentrating low distances around the diagonal. Uses the "SPIN_NH" method in `seriation`
#' @param sigma The variance around the diagonal to use for the weight matrix. Either a single number or a decreasing sequence.
#' @importFrom rlang enquo eval_tidy
#' @export
node_rank_spin_in <- function(step = 5, sigma = seq(20, 1, length.out = 10), dist = 'shortest', mode = 'out', weights = NULL, algorithm = 'automatic') {
  expect_nodes()
  weights <- enquo(weights)
  weights <- eval_tidy(weights, .E())
  if (is.null(weights)) {
    weights <- NA
  }
  mat <- to_dist(.G(), dist, mode, weights, algorithm)
  seriate(mat, 'SPIN_NH', list(step = step, sigma = sigma))
}
#' @describeIn node_rank Use quadratic assignment problem formulations to minimize criterions using simulated annealing. Uses the "QAP_LS", "QAP_2SUM", "QAP_BAR", or "QAP_Inertia" methods from `seriation` dependant on the `criterion` argument
#' @param criterion The criterion to minimize. Either "LS" (Linear Seriation Problem), "2SUM" (2-Sum Problem), "BAR" (Banded Anti-Robinson form), or "Inertia" (Inertia criterion)
#' @param temp_multiplier Temperature multiplication factor between 0 and 1
#' @param maxsteps The upper bound of iterations
#' @importFrom rlang enquo eval_tidy
#' @export
node_rank_quadratic <- function(criterion = '2SUM', reps = 1, step = 2 * graph_order(), step_multiplier = 1.1, temp_multiplier  = 0.5, maxsteps = 50, dist = 'shortest', mode = 'out', weights = NULL, algorithm = 'automatic') {
  expect_nodes()
  weights <- enquo(weights)
  weights <- eval_tidy(weights, .E())
  if (is.null(weights)) {
    weights <- NA
  }
  mat <- to_dist(.G(), dist, mode, weights, algorithm)
  control = list(rep = reps, miter = step, fiter = step_multiplier, ft  = temp_multiplier, maxsteps = maxsteps)
  method = paste0('QAP_', toupper(criterion))
  seriate(mat, method, control)
}
#' @describeIn node_rank Optimizes different criteria based on a genetic algorithm. Uses the "GA" method from `seriation`. See `register_GA` for an overview of relevant arguments
#' @importFrom rlang enquo eval_tidy
#' @export
node_rank_genetic <- function(... , dist = 'shortest', mode = 'out', weights = NULL, algorithm = 'automatic') {
  expect_nodes()
  weights <- enquo(weights)
  weights <- eval_tidy(weights, .E())
  if (is.null(weights)) {
    weights <- NA
  }
  mat <- to_dist(.G(), dist, mode, weights, algorithm)
  seriate(mat, 'GA', list())
}
#' @describeIn node_rank Optimizes different criteria based on heuristic dendrogram seriation. Uses the "DendSer" method from `seriation`. See `register_DendSer` for an overview of relevant arguments
#' @importFrom rlang enquo eval_tidy
#' @export
node_rank_dendser <- function(... , dist = 'shortest', mode = 'out', weights = NULL, algorithm = 'automatic') {
  expect_nodes()
  weights <- enquo(weights)
  weights <- eval_tidy(weights, .E())
  if (is.null(weights)) {
    weights <- NA
  }
  mat <- to_dist(.G(), dist, mode, weights, algorithm)
  seriate(mat, 'DendSer', list())
}

# HELPERS -----------------------------------------------------------------

#' @importFrom igraph distances as_adjacency_matrix
#' @importFrom stats dist as.dist
to_dist <- function(graph, dist, mode, weights, algorithm) {
  if (is.function(dist)) {
    mat <- dist(graph)
    if (!inherits(mat, 'dist')) cli::cli_abort('{.arg dist} must return a {cls dist} object')
    if (attr(mat, 'Size') != gorder(graph)) cli::cli_abort('{.arg dist} must return a {.cls dist} object of the same size as the order of {.arg graph}')
  } else if (is.character(dist)) {
    if (dist == 'shortest') {
      mat <- distances(graph, mode = mode, weights = weights, algorithm = algorithm)
      mat <- as.dist(mat)
    } else {
      mat <- as_adjacency_matrix(graph, type = 'both', sparse = FALSE)
      if (mode == 'out') mat <- dist(mat, dist)
      if (mode == 'in') mat <- dist(t(mat), dist)
      if (mode == 'both') mat <- (dist(mat, dist) + dist(t(mat), dist))/2
    }
  }
  mat
}

seriate <- function(mat, method, control) {
  expect_seriation()
  if (method == 'GA') seriation::register_GA()
  if (method == 'DendSer') seriation::register_DendSer()
  ser <- seriation::seriate(mat, method, control)
  seriation::get_rank(ser)
}
