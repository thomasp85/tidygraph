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
#' @param n_groups Integer scalar, the desired number of communities. If too low
#' or two high, then an error message is given. The measure is applied to the
#' full graph so the number of groups returned may be lower for focused graphs
#' @param trials Number of times partition of the network should be attempted
#' @param steps The number of steps in the random walks
#' @param options Settings passed on to `igraph::arpack()`
#' @param resolution Resolution of the modularity function used internally in
#' the algorithm
#' @param objective_function Either `"CPM"` (constant potts model) or
#' `"modularity"`. Sets the objective function to use.
#' @param beta Parameter affecting the randomness in the Leiden algorithm. This
#' affects only the refinement step of the algorithm.
#' @param n The number of iterations to run the clustering
#' @param ... arguments passed on to [igraph::cluster_spinglass()]
#'
#' @return a numeric vector with the membership for each node in the graph. The
#' enumeration happens in order based on group size progressing from the largest
#' to the smallest group
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
  group <- as.integer(components(graph = .G(), mode = type)$membership[focus_ind(.G(), 'nodes')])
  desc_enumeration(group)
}
#' @describeIn group_graph Group densely connected nodes using [igraph::cluster_edge_betweenness()]
#' @importFrom igraph membership cluster_edge_betweenness cut_at
#' @export
group_edge_betweenness <- function(weights = NULL, directed = TRUE, n_groups = NULL) {
  expect_nodes()
  weights <- enquo(weights)
  weights <- eval_tidy(weights, .E()) %||% NA
  group <- cluster_edge_betweenness(graph = .G(), weights = weights, directed = directed)
  if (is.null(n_groups)) {
    group <- membership(group)
  } else {
    group <- cut_at(group, no = n_groups)
  }
  group <- as.integer(group[focus_ind(.G(), 'nodes')])
  desc_enumeration(group)
}
#' @describeIn group_graph Group nodes by optimising modularity using [igraph::cluster_fast_greedy()]
#' @importFrom igraph membership cluster_fast_greedy cut_at
#' @export
group_fast_greedy <- function(weights = NULL, n_groups = NULL) {
  expect_nodes()
  weights <- enquo(weights)
  weights <- eval_tidy(weights, .E()) %||% NA
  group <- cluster_fast_greedy(graph = .G(), weights = weights)
  if (is.null(n_groups)) {
    group <- membership(group)
  } else {
    group <- cut_at(group, no = n_groups)
  }
  group <- as.integer(group[focus_ind(.G(), 'nodes')])
  desc_enumeration(group)
}
#' @describeIn group_graph Group nodes by minimizing description length using [igraph::cluster_infomap()]
#' @importFrom igraph membership cluster_infomap
#' @export
group_infomap <- function(weights = NULL, node_weights = NULL, trials = 10) {
  expect_nodes()
  weights <- enquo(weights)
  weights <- eval_tidy(weights, .E()) %||% NA
  node_weights <- enquo(node_weights)
  node_weights <- eval_tidy(node_weights, .N())
  group <- as.integer(membership(cluster_infomap(graph = .G(), e.weights = weights, v.weights = node_weights, nb.trials = trials))[focus_ind(.G(), 'nodes')])
  desc_enumeration(group)
}
#' @describeIn group_graph Group nodes by propagating labels using [igraph::cluster_label_prop()]
#' @importFrom igraph membership cluster_label_prop
#' @export
group_label_prop <- function(weights = NULL, label = NULL, fixed = NULL) {
  expect_nodes()
  weights <- enquo(weights)
  weights <- eval_tidy(weights, .E()) %||% NA
  N <- .N()
  label <- enquo(label)
  label <- eval_tidy(label, N)
  fixed <- enquo(fixed)
  fixed <- eval_tidy(fixed, N)
  group <- as.integer(membership(cluster_label_prop(graph = .G(), weights = weights, initial = label, fixed = fixed))[focus_ind(.G(), 'nodes')])
  desc_enumeration(group)
}
#' @describeIn group_graph Group nodes based on the leading eigenvector of the modularity matrix using [igraph::cluster_leading_eigen()]
#' @importFrom igraph membership cluster_leading_eigen cut_at arpack_defaults
#' @export
group_leading_eigen <- function(weights = NULL, steps = -1, label = NULL, options = arpack_defaults(), n_groups = NULL) {
  expect_nodes()
  weights <- enquo(weights)
  weights <- eval_tidy(weights, .E()) %||% NA
  label <- enquo(label)
  label <- eval_tidy(label, .N())
  group <- cluster_leading_eigen(graph = .G(), steps = steps, weights = weights, start = label, options = options)
  if (is.null(n_groups)) {
    group <- membership(group)
  } else {
    group <- cut_at(group, no = n_groups)
  }
  group <- as.integer(group[focus_ind(.G(), 'nodes')])
  desc_enumeration(group)
}
#' @describeIn group_graph Group nodes by multilevel optimisation of modularity using [igraph::cluster_louvain()]
#' @importFrom igraph membership cluster_louvain
#' @export
group_louvain <- function(weights = NULL, resolution = 1) {
  expect_nodes()
  weights <- enquo(weights)
  weights <- eval_tidy(weights, .E()) %||% NA
  group <- as.integer(membership(cluster_louvain(graph = .G(), weights = weights, resolution = resolution))[focus_ind(.G(), 'nodes')])
  desc_enumeration(group)
}
#' @describeIn group_graph Group nodes according to the Leiden algorithm ([igraph::cluster_leiden()]) which is similar, but more efficient and provides higher quality results than `cluster_louvain()`
#' @importFrom igraph membership cluster_leiden
#' @importFrom rlang list2 inject :=
#' @export
group_leiden <- function(weights = NULL, resolution = 1, objective_function = 'CPM', beta = 0.01, label = NULL, n = 2, node_weights = NULL) {
  expect_nodes()
  graph <- .G()
  weights <- enquo(weights)
  weights <- eval_tidy(weights, .E()) %||% NA
  # `resolution_parameter` may be renamed to `resolution` in future release of igraph
  res_arg <- if ("resolution" %in% names(formals(igraph::cluster_leiden))) {
    'resolution'
  } else {
    'resolution_parameter'
  }
  nodes <- .N(focused = FALSE)
  label <- enquo(label)
  label <- eval_tidy(label, nodes)
  node_weights <- enquo(node_weights)
  node_weights <- eval_tidy(node_weights, nodes)
  args <- list2(
    graph = graph,
    objective_function = objective_function,
    weights = weights,
    {{res_arg}} := resolution,
    beta = beta,
    initial_membership = label,
    n_iterations = n,
    vertex_weights = node_weights
  )

  group <- as.integer(membership(inject(cluster_leiden(!!!args)))[focus_ind(.G(), 'nodes')])
  desc_enumeration(group)
}
#' @describeIn group_graph Group nodes by optimising the moldularity score using [igraph::cluster_optimal()]
#' @importFrom igraph membership cluster_optimal
#' @export
group_optimal <- function(weights = NULL) {
  expect_nodes()
  weights <- enquo(weights)
  weights <- eval_tidy(weights, .E()) %||% NA
  group <- as.integer(membership(cluster_optimal(graph = .G(), weights = weights))[focus_ind(.G(), 'nodes')])
  desc_enumeration(group)
}
#' @describeIn group_graph Group nodes using simulated annealing with [igraph::cluster_spinglass()]
#' @importFrom igraph membership cluster_spinglass
#' @export
group_spinglass <- function(weights = NULL, ...) {
  expect_nodes()
  weights <- enquo(weights)
  weights <- eval_tidy(weights, .E()) %||% NA
  group <- as.integer(membership(cluster_spinglass(graph = .G(), weights = weights, vertex = NULL, ...))[focus_ind(.G(), 'nodes')])
  desc_enumeration(group)
}
#' @describeIn group_graph Group nodes via short random walks using [igraph::cluster_walktrap()]
#' @importFrom igraph membership cluster_walktrap cut_at
#' @export
group_walktrap <- function(weights = NULL, steps = 4, n_groups = NULL) {
  expect_nodes()
  weights <- enquo(weights)
  weights <- eval_tidy(weights, .E()) %||% NA
  group <- cluster_walktrap(graph = .G(), weights = weights, steps = steps)
  if (is.null(n_groups)) {
    group <- membership(group)
  } else {
    group <- cut_at(group, no = n_groups)
  }
  group <- as.integer(group[focus_ind(.G(), 'nodes')])
  desc_enumeration(group)
}
#' @describeIn group_graph Group nodes by simulating fluid interactions on the graph topology using [igraph::cluster_fluid_communities()]
#' @importFrom igraph cluster_fluid_communities
#' @export
group_fluid <- function(n_groups = 2) {
  expect_nodes()
  graph <- .G()
  group <- as.integer(membership(cluster_fluid_communities(graph, n_groups))[focus_ind(graph, 'nodes')])
  desc_enumeration(group)
}
#' @describeIn group_graph Group edges by their membership of the maximal binconnected components using [igraph::biconnected_components()]
#' @importFrom igraph biconnected_components
#' @export
group_biconnected_component <- function() {
  expect_edges()
  graph <- .G()
  comp <- biconnected_components(graph)
  ind <- lapply(comp$component_edges, as.integer)
  group <- rep(seq_along(ind), lengths(ind))[order(unlist(ind))][focus_ind(.G(), 'edges')]
  desc_enumeration(group)
}
#' @describeIn group_graph Groups nodes by their color using [igraph::greedy_vertex_coloring()]. Be aware that this is not a clustering algorithm as coloring specifically provide a color to each node so that no neighbors have the same color
#' @importFrom igraph greedy_vertex_coloring
#' @export
group_color <- function() {
  expect_nodes()
  graph <- .G()
  group <- greedy_vertex_coloring(graph)

  group <- as.integer(group[focus_ind(.G(), 'nodes')])
  desc_enumeration(group)
}


# HELPERS -----------------------------------------------------------------

# Take an integer vector and recode it so the most prevalent integer is 1 and so
# forth
desc_enumeration <- function(group) {
  match(group, as.integer(names(sort(table(group), decreasing = TRUE))))
}
