#' Querying node types
#'
#' These functions all lets the user query whether each node is of a certain
#' type. All of the functions returns a logical vector indicating whether the
#' node is of the type in question. Do note that the types are not mutually
#' exclusive and that nodes can thus be of multiple types.
#'
#' @param mode The way edges should be followed in the case of directed graphs.
#'
#' @return A logical vector of the same length as the number of nodes in the
#' graph.
#'
#' @name node_types
#' @rdname node_types
#'
#' @examples
#' # Find the root and leafs in a tree
#' create_tree(40, 2) %>%
#'   mutate(root = node_is_root(), leaf = node_is_leaf())
NULL

#' @describeIn node_types is the node a cut node (articaultion node)
#' @importFrom igraph gorder articulation_points
#' @export
node_is_cut <- function() {
  expect_nodes()
  graph <- .G()
  focus_ind(graph, 'nodes') %in% articulation_points(graph)
}
#' @describeIn node_types is the node a root in a tree
#' @importFrom igraph degree is.directed
#' @export
node_is_root <- function() {
  expect_nodes()
  graph <- .G()
  node_inds <- focus_ind(graph, 'nodes')
  if ((!is_tree(graph) && !is_forest(graph)) || !is.directed(graph)) {
    return(rep(FALSE, length(node_inds)))
  }
  deg_in <- degree(graph, mode = 'in') == 0
  deg_out <- degree(graph, mode = 'out') == 0
  root <- if (sum(deg_in) > sum(deg_out)) deg_out else deg_in
  root[node_inds]
}
#' @describeIn node_types is the node a leaf in a tree
#' @importFrom igraph degree is.directed
#' @export
node_is_leaf <- function() {
  expect_nodes()
  graph <- .G()
  node_inds <- focus_ind(graph, 'nodes')
  if ((!is_tree(graph) && !is_forest(graph))) {
    return(rep(FALSE, length(node_inds)))
  }
  if (is.directed(graph)) {
    deg_in <- degree(graph, mode = 'in') == 0
    deg_out <- degree(graph, mode = 'out') == 0
    leaf <- if (sum(deg_out) > sum(deg_in)) deg_out else deg_in
    leaf[node_inds]
  } else {
    degree(graph, v = node_inds, mode = 'all') == 1
  }
}
#' @describeIn node_types does the node only have incomming edges
#' @importFrom igraph degree
#' @export
node_is_sink <- function() {
  expect_nodes()
  graph <- .G()
  node_inds <- focus_ind(graph, 'nodes')
  deg_in <- degree(graph, v = node_inds, mode = 'in')
  deg_out <- degree(graph, v = node_inds, mode = 'out')
  deg_out == 0 & deg_in != 0
}
#' @describeIn node_types does the node only have outgoing edges
#' @importFrom igraph degree
#' @export
node_is_source <- function() {
  expect_nodes()
  graph <- .G()
  node_inds <- focus_ind(graph, 'nodes')
  deg_in <- degree(graph, v = node_inds, mode = 'in')
  deg_out <- degree(graph, v = node_inds, mode = 'out')
  deg_out != 0 & deg_in == 0
}
#' @describeIn node_types is the node unconnected
#' @importFrom igraph degree
#' @export
node_is_isolated <- function() {
  expect_nodes()
  graph <- .G()
  degree(graph, v = focus_ind(graph, 'nodes')) == 0
}
#' @describeIn node_types is the node connected to all other nodes in the graph
#' @importFrom igraph ego_size gorder
#' @export
node_is_universal <- function(mode = 'out') {
  expect_nodes()
  graph <- .G()
  ego_size(graph, order = 1, nodes = focus_ind(graph, 'nodes'), mode = mode) == gorder(graph)
}
#' @describeIn node_types are all the neighbors of the node connected
#' @importFrom igraph local_scan ecount ego_size
#' @export
node_is_simplical <- function(mode = 'out') {
  expect_nodes()
  graph <- .G()
  node_inds <- focus_ind(graph, 'nodes')
  n_edges <- local_scan(graph, k = 1, mode = mode, FUN = ecount)[node_inds]
  n_nodes <- ego_size(graph, order = 1, nodes = node_inds, mode = mode)
  n_edges == n_nodes * (n_nodes - 1) * 0.5
}
#' @describeIn node_types does the node have the minimal eccentricity in the graph
#' @importFrom igraph eccentricity
#' @export
node_is_center <- function(mode = 'out') {
  expect_nodes()
  graph <- .G()
  ecc <- eccentricity(graph, mode = mode)
  ecc[focus_ind(graph, 'nodes')] == min(ecc)
}
#' @describeIn node_types is a node adjacent to any of the nodes given in `to`
#' @param to The nodes to test for adjacency to
#' @param include_to Should the nodes in `to` be marked as adjacent as well
#' @importFrom igraph adjacent_vertices
#' @export
node_is_adjacent <- function(to, mode = 'all', include_to = TRUE) {
  expect_nodes()
  graph <- .G()
  to <- as_node_ind(to, graph)
  include <- unlist(adjacent_vertices(graph, to, mode))
  if (include_to) include <- union(to, include)
  focus_ind(graph, 'nodes') %in% include
}
#' @describeIn node_types Is a node part of the keyplayers in the graph (`influenceR`)
#' @param k The number of keyplayers to identify
#' @param p The probability to accept a lesser state
#' @param tol Optimisation tolerance, below which the optimisation will stop
#' @param maxsec The total computation budget for the optimization, in seconds
#' @param roundsec Number of seconds in between synchronizing workers' answer
#' @importFrom igraph gorder
#' @export
node_is_keyplayer <- function(k, p = 0, tol = 1e-4, maxsec = 120, roundsec = 30) {
  expect_influencer()
  expect_nodes()
  graph <- .G()
  ind <- influenceR::keyplayer(graph, k = k, prob = p, tol = tol, maxsec = maxsec, roundsec = roundsec)
  focus_ind(graph, 'nodes') %in% ind
}
#' @describeIn node_types Is a node connected to all (or any) nodes in a set
#' @param nodes The set of nodes to test connectivity to. Can be a list to use
#' different sets for different nodes. If a list it will be recycled as
#' necessary.
#' @param any Logical. If `TRUE` the node only needs to be connected to a single
#' node in the set for it to return `TRUE`
#' @importFrom igraph distances gorder
#' @export
node_is_connected <- function(nodes, mode = 'all', any = FALSE) {
  expect_nodes()
  graph <- .G()
  node_inds <- focus_ind(graph, 'nodes')
  if (!is.list(nodes)) nodes <- list(nodes)
  all_nodes <- unique(unlist(nodes))
  reached <- is.finite(t(distances(graph, v = node_inds, to = all_nodes, mode = mode, weights = NA)))
  nodes <- rep_len(nodes, length(node_inds))
  vapply(seq_along(node_inds), function(i) {
    n <- node_inds[i]
    connections <- match(nodes[[i]], all_nodes)
    found <- reached[,n][connections]
    if (any) any(found) else all(found)
  }, logical(1))
}
#' Querying node measures
#'
#' These functions are a collection of node measures that do not really fall
#' into the class of [centrality] measures. For lack of a better place they are
#' collected under the `node_*` umbrella of functions.
#'
#' @param mode How edges are treated. In `node_coreness()` it chooses which kind
#' of coreness measure to calculate. In `node_efficiency()` it defines how the
#' local neighborhood is created
#' @param weights The weights to use for each node during calculation
#' @param directed Should the graph be treated as a directed graph if it is in
#' fact directed
#'
#' @return A numeric vector of the same length as the number of nodes in the
#' graph.
#'
#' @name node_measures
#' @rdname node_measures
#'
#' @examples
#' # Calculate Burt's Constraint for each node
#' create_notable('meredith') %>%
#'   mutate(b_constraint = node_constraint())
NULL

#' @describeIn node_measures measure the maximum shortest path to all other nodes in the graph
#' @importFrom igraph eccentricity
#' @export
node_eccentricity <- function(mode = 'out') {
  expect_nodes()
  graph <- .G()
  eccentricity(graph, focus_ind(graph, 'nodes'), mode = mode)
}
#' @describeIn node_measures measures Burts constraint of the node. See [igraph::constraint()]
#' @importFrom igraph constraint
#' @export
node_constraint <- function(weights = NULL) {
  expect_nodes()
  graph <- .G()
  weights <- enquo(weights)
  weights <- eval_tidy(weights, .E())
  if (is.null(weights)) {
    weights <- rep_len(1L, gsize(graph))
  }
  constraint(graph, focus_ind(graph, 'nodes'), weights = weights)
}
#' @describeIn node_measures measures the coreness of each node. See [igraph::coreness()]
#' @importFrom igraph coreness
#' @export
node_coreness <- function(mode = 'out') {
  expect_nodes()
  graph <- .G()
  coreness(graph, mode = mode)[focus_ind(graph, 'nodes')]
}
#' @describeIn node_measures measures the diversity of the node. See [igraph::diversity()]
#' @importFrom igraph diversity
#' @export
node_diversity <- function(weights) {
  expect_nodes()
  graph <- .G()
  weights <- enquo(weights)
  weights <- eval_tidy(weights, .E())
  if (is.null(weights)) {
    cli::cli_abort('{.arg weights} must be a valid vector')
  }
  diversity(graph, weights = weights, vids = focus_ind(graph, 'nodes'))
}
#' @describeIn node_measures measures the local efficiency around each node. See [igraph::local_efficiency()]
#' @importFrom rlang enquo eval_tidy
#' @importFrom igraph local_efficiency
#' @export
node_efficiency <- function(weights = NULL, directed = TRUE, mode = 'all') {
  expect_nodes()
  graph <- .G()
  weights <- enquo(weights)
  weights <- eval_tidy(weights, .E()) %||% NA
  local_efficiency(graph, focus_ind(graph, 'nodes'), weights, directed, mode)
}
#' @describeIn node_measures measures Valente's Bridging measures for detecting structural bridges (`influenceR`)
#' @export
node_bridging_score <- function() {
  expect_influencer()
  expect_nodes()
  graph <- .G()
  influenceR::bridging(graph)[focus_ind(graph, 'nodes')]
}
#' @describeIn node_measures measures Burt's Effective Network Size indicating access to structural holes in the network (`influenceR`)
#' @export
node_effective_network_size <- function() {
  expect_influencer()
  expect_nodes()
  graph <- .G()
  influenceR::ens(graph)[focus_ind(graph, 'nodes')]
}
#' @describeIn node_measures measures the impact on connectivity when removing the node (`NetSwan`)
#' @export
node_connectivity_impact <- function() {
  expect_netswan()
  expect_nodes()
  graph <- .G()
  NetSwan::swan_connectivity(graph)[focus_ind(graph, 'nodes')]
}
#' @describeIn node_measures measures the impact on closeness when removing the node (`NetSwan`)
#' @export
node_closeness_impact <- function() {
  expect_netswan()
  expect_nodes()
  graph <- .G()
  NetSwan::swan_closeness(graph)[focus_ind(graph, 'nodes')]
}
#' @describeIn node_measures measures the impact on fareness (distance between all node pairs) when removing the node (`NetSwan`)
#' @export
node_fareness_impact <- function() {
  expect_netswan()
  expect_nodes()
  graph <- .G()
  NetSwan::swan_efficiency(graph)[focus_ind(graph, 'nodes')]
}
