#' Group nodes and edges based on community structure
#'
#' These functions are wrappers around the various clustering functions provided
#' by `igraph`. As with the other wrappers they automatically use the graph that
#' is being computed on, and otherwise passes on its arguments to the relevant
#' clustering function. The return value is always a numeric vector of group
#' memberships so that nodes or edges with the same number are part of the same
#' group. Grouping is predominantly made on nodes and currently the only
#' grouping of edges supported is biconnected components.
#'
#' @param weights The weight of the edges to use for the calculation. Will be
#' evaluated in the context of the edge data.
#' @param node_weights The weight of the nodes to use for the calculation. Will
#' be evaluated in the context of the node data.
#' @param label The initial groups of the nodes. Will be evaluated in the
#' context of the node data.
#' @param fixed A logical vector determining which nodes should keep their
#' initial groups. Will be evaluated in the context of the node data.
#' @param type The type of component to find. Either `'weak'` or `'strong'`
#' @param directed Should direction of edges be used for the calculations
#' @param trials Number of times partition of the network should be attempted
#' @param steps The number of steps in the random walks
#' @param options Settings passed on to `igraph::arpack()`
#' @param ... arguments passed on to [igraph::cluster_spinglass()]
#'
#' @return a numeric vector with the membership for each node in the graph
#'
#' @name group_graph
#' @rdname group_graph
#'
#' @examples
#' create_notable('tutte') %>%
#'   activate(nodes) %>%
#'   mutate(group = group_infomap())
#'
NULL

#' @describeIn group_graph Group by connected compenents using [igraph::components()]
#' @importFrom igraph components
#' @export
group_components <- function(type = 'weak') {
  expect_nodes()
  as.integer(components(graph = .G(), mode = type)$membership)
}
#' @describeIn group_graph Group densely connected nodes using [igraph::cluster_edge_betweenness()]
#' @importFrom igraph membership cluster_edge_betweenness
#' @export
group_edge_betweenness <- function(weights = NULL, directed = TRUE) {
  expect_nodes()
  weights <- enquo(weights)
  weights <- eval_tidy(weights, .E())
  as.integer(membership(cluster_edge_betweenness(graph = .G(), weights = weights, directed = directed)))
}
#' @describeIn group_graph Group nodes by optimising modularity using [igraph::cluster_fast_greedy()]
#' @importFrom igraph membership cluster_fast_greedy
#' @export
group_fast_greedy <- function(weights = NULL) {
  expect_nodes()
  weights <- enquo(weights)
  weights <- eval_tidy(weights, .E())
  as.integer(membership(cluster_fast_greedy(graph = .G(), weights = weights)))
}
#' @describeIn group_graph Group nodes by minimizing description length using [igraph::cluster_infomap()]
#' @importFrom igraph membership cluster_infomap
#' @export
group_infomap <- function(weights = NULL, node_weights = NULL, trials = 10) {
  expect_nodes()
  weights <- enquo(weights)
  weights <- eval_tidy(weights, .E())
  node_weights <- enquo(node_weights)
  node_weights <- eval_tidy(node_weights, .N())
  as.integer(membership(cluster_infomap(graph = .G(), e.weights = weights, v.weights = node_weights, nb.trials = trials)))
}
#' @describeIn group_graph Group nodes by propagating labels using [igraph::cluster_label_prop()]
#' @importFrom igraph membership cluster_label_prop
#' @export
group_label_prop <- function(weights = NULL, label = NULL, fixed = NULL) {
  expect_nodes()
  weights <- enquo(weights)
  weights <- eval_tidy(weights, .E())
  N <- .N()
  label <- enquo(label)
  label <- eval_tidy(label, N)
  fixed <- enquo(fixed)
  fixed <- eval_tidy(fixed, N)
  as.integer(membership(cluster_label_prop(graph = .G(), weights = weights, initial = label, fixed = fixed)))
}
#' @describeIn group_graph Group nodes based on the leading eigenvector of the modularity matrix using [igraph::cluster_leading_eigen()]
#' @importFrom igraph membership cluster_leading_eigen
#' @export
group_leading_eigen <- function(weights = NULL, steps = -1, label = NULL, options = igraph::arpack_defaults) {
  expect_nodes()
  weights <- enquo(weights)
  weights <- eval_tidy(weights, .E())
  label <- enquo(label)
  label <- eval_tidy(label, .N())
  as.integer(membership(cluster_leading_eigen(graph = .G(), steps = steps, weights = weights, start = label, options = options)))
}
#' @describeIn group_graph Group nodes by multilevel optimisation of modularity using [igraph::cluster_louvain()]
#' @importFrom igraph membership cluster_louvain
#' @export
group_louvain <- function(weights = NULL) {
  expect_nodes()
  weights <- enquo(weights)
  weights <- eval_tidy(weights, .E())
  as.integer(membership(cluster_louvain(graph = .G(), weights = weights)))
}
#' @describeIn group_graph Group nodes by optimising the moldularity score using [igraph::cluster_optimal()]
#' @importFrom igraph membership cluster_optimal
#' @export
group_optimal <- function(weights = NULL) {
  expect_nodes()
  weights <- enquo(weights)
  weights <- eval_tidy(weights, .E())
  as.integer(membership(cluster_optimal(graph = .G(), weights = weights)))
}
#' @describeIn group_graph Group nodes using simulated annealing with [igraph::cluster_spinglass()]
#' @importFrom igraph membership cluster_spinglass
#' @export
group_spinglass <- function(weights = NULL, ...) {
  expect_nodes()
  weights <- enquo(weights)
  weights <- eval_tidy(weights, .E())
  as.integer(membership(cluster_spinglass(graph = .G(), weights = weights, vertex = NULL, ...)))
}
#' @describeIn group_graph Group nodes via short random walks using [igraph::cluster_walktrap()]
#' @importFrom igraph membership cluster_walktrap
#' @export
group_walktrap <- function(weights = NULL, steps = 4) {
  expect_nodes()
  weights <- enquo(weights)
  weights <- eval_tidy(weights, .E())
  as.integer(membership(cluster_walktrap(graph = .G(), weights = weights, steps = steps)))
}
#' @describeIn group_graph Group edges by their membership of the maximal binconnected components using [igraph::biconnected_components()]
#' @importFrom igraph biconnected_components
#' @export
group_biconnected_component <- function() {
  expect_edges()
  graph <- .G()
  comp <- biconnected_components(graph)
  ind <- lapply(comp$component_edges, as.integer)
  rep(seq_along(ind), lengths(ind))[order(unlist(ind))]
}
