#' Graph measurements
#'
#' This set of functions provide wrappers to a number of `ìgraph`s graph
#' statistic algorithms. As for the other wrappers provided, they are intended
#' for use inside the `tidygraph` framework and it is thus not necessary to
#' supply the graph being computed on as the context is known. All of these
#' functions are guarantied to return scalars making it easy to compute with
#' them.
#'
#' @return A scalar, the type depending on the function
#'
#' @name graph_measures
#' @rdname graph_measures
#'
#' @examples
#' # Use e.g. to modify computations on nodes and edges
#' create_notable('meredith') %>%
#'   activate(nodes) %>%
#'   mutate(rel_neighbors = centrality_degree()/graph_order())
NULL

#' @describeIn graph_measures Gives the minimum edge connectivity. Wraps [igraph::edge_connectivity()]
#' @importFrom igraph edge_connectivity
#' @export
graph_adhesion <- function() {
  graph <- .G()
  edge_connectivity(graph)
}
#' @describeIn graph_measures Measures the propensity of similar nodes to be connected. Wraps [igraph::assortativity()]
#' @param attr The node attribute to measure on
#' @param in_attr An alternative node attribute to use for incomming node. If `NULL` the attribute given by `type` will be used
#' @param directed Should a directed graph be treated as directed
#' @importFrom igraph assortativity assortativity_nominal
#' @export
graph_assortativity <- function(attr, in_attr = NULL, directed = TRUE) {
  graph <- .G()
  attr <- enquo(attr)
  attr <- eval_tidy(attr, .N(focused = FALSE))
  if (is.numeric(attr)) {
    in_attr <- enquo(in_attr)
    in_attr <- eval_tidy(in_attr, .N(focused = FALSE))
    assortativity(graph, values = attr, values.in = in_attr, directed = directed)
  } else {
    assortativity_nominal(graph, types = as.factor(attr), directed = directed)
  }
}
#' @describeIn graph_measures Calculate the number of automorphisms of the graph. Wraps [igraph::count_automorphisms()]
#' @inheritParams igraph::count_automorphisms
#' @importFrom igraph count_automorphisms
#' @export
graph_automorphisms <- function(sh = 'fm', colors = NULL) {
  graph <- .G()
  colors <- enquo(colors)
  colors <- eval_tidy(colors, .N(focused = FALSE))
  as.numeric(count_automorphisms(graph, colors = colors, sh = sh)$group_size)
}
#' @describeIn graph_measures Get the size of the largest clique. Wraps [igraph::clique_num()]
#' @importFrom igraph clique_num
#' @export
graph_clique_num <- function() {
  graph <- .G()
  clique_num(graph)
}
#' @describeIn graph_measures Get the number of maximal cliques in the graph. Wraps [igraph::count_max_cliques()]
#' @param min,max The upper and lower bounds of the cliques to be considered.
#' @param subset The indexes of the nodes to start the search from (logical or integer). If provided only the cliques containing these nodes will be counted.
#' @importFrom igraph count_max_cliques
#' @export
graph_clique_count <- function(min = NULL, max = NULL, subset = NULL) {
  graph <- .G()
  subset <- enquo(subset)
  subset <- eval_tidy(subset, .N(focused = FALSE))
  if (is.logical(subset)) subset <- which(subset)
  count_max_cliques(graph, min, max, subset)
}
#' @describeIn graph_measures Count the number of unconnected componenets in the graph. Wraps [igraph::count_components()]
#' @param type The type of component to count, either 'weak' or 'strong'. Ignored for undirected graphs.
#' @importFrom igraph count_components
#' @export
graph_component_count <- function(type = 'weak') {
  graph <- .G()
  count_components(graph, type)
}
#' @describeIn graph_measures Count the number of motifs in a graph. Wraps [igraph::count_motifs()]
#' @inheritParams igraph::count_motifs
#' @importFrom igraph count_motifs
#' @export
graph_motif_count <- function(size = 3, cut.prob = rep(0, size)) {
  graph <- .G()
  count_motifs(graph, size, cut.prob)
}
#' @describeIn graph_measures Measures the length of the longest geodesic. Wraps [igraph::diameter()]
#' @inheritParams igraph::diameter
#' @importFrom igraph diameter
#' @export
graph_diameter <- function(weights = NULL, directed = TRUE, unconnected = TRUE) {
  graph <- .G()
  weights <- enquo(weights)
  weights <- eval_tidy(weights, .E(focused = FALSE)) %||% NA
  diameter(graph, directed, unconnected, weights)
}
#' @describeIn graph_measures Measrues the length of the shortest circle in the graph. Wraps [igraph::girth()]
#' @importFrom igraph girth
#' @export
graph_girth <- function() {
  graph <- .G()
  girth(graph, F)$girth
}
#' @describeIn graph_measures Measures the smallest eccentricity in the graph. Wraps [igraph::radius()]
#' @param mode How should eccentricity be calculated. If `"out"` only outbound edges are followed. If `"in"` only inbound are followed. If `"all"` all edges are followed. Ignored for undirected graphs.
#' @importFrom igraph radius
#' @export
graph_radius <- function(mode = 'out') {
  graph <- .G()
  radius(graph, mode)
}
#' @describeIn graph_measures Counts the number of mutually connected nodes. Wraps [igraph::dyad_census()]
#' @importFrom igraph dyad_census
#' @export
graph_mutual_count <- function() {
  graph <- .G()
  unname(dyad_census(graph)['mut'])
}
#' @describeIn graph_measures Counts the number of asymmetrically connected nodes. Wraps [igraph::dyad_census()]
#' @importFrom igraph dyad_census
#' @export
graph_asym_count <- function() {
  graph <- .G()
  unname(dyad_census(graph)['asym'])
}
#' @describeIn graph_measures Counts the number of unconnected node pairs. Wraps [igraph::dyad_census()]
#' @importFrom igraph dyad_census
#' @export
graph_unconn_count <- function() {
  graph <- .G()
  unname(dyad_census(graph)['null'])
}
#' @describeIn graph_measures Counts the number of edges in the graph. Wraps [igraph::gsize()]
#' @importFrom igraph gsize
#' @export
graph_size <- function() {
  graph <- .G()
  gsize(graph)
}
#' @describeIn graph_measures Counts the number of nodes in the graph. Wraps [igraph::gorder()]
#' @importFrom igraph gorder
#' @export
graph_order <- function() {
  graph <- .G()
  gorder(graph)
}
#' @describeIn graph_measures Measures the proportion of mutual connections in the graph. Wraps [igraph::reciprocity()]
#' @param ignore_loops Logical. Should loops be ignored while calculating the reciprocity
#' @param ratio Should the old "ratio" approach from igraph < v0.6 be used
#' @importFrom igraph reciprocity
#' @export
graph_reciprocity <- function(ignore_loops = TRUE, ratio = FALSE) {
  graph <- .G()
  reciprocity(graph, ignore_loops, mode = if (ratio) 'ratio' else 'default')
}
#' @describeIn graph_measures Calculates the minimum number of edges to remove in order to split the graph into two clusters. Wraps [igraph::min_cut()]
#' @param capacity The capacity of the edges
#' @importFrom igraph min_cut
#' @export
graph_min_cut <- function(capacity = NULL) {
  graph <- .G()
  capacity <- enquo(capacity)
  capacity <- eval_tidy(capacity, .E(focused = FALSE))
  min_cut(graph, capacity = capacity)
}
#' @describeIn graph_measures Calculates the mean distance between all node pairs in the graph. Wraps [igraph::mean_distance()]
#' @importFrom igraph mean_distance
#' @export
graph_mean_dist <- function(directed = TRUE, unconnected = TRUE, weights = NULL) {
  graph <- .G()
  weights <- enquo(weights)
  weights <- eval_tidy(weights, .E(focused = FALSE)) %||% NA
  mean_distance(graph, directed = directed, unconnected = unconnected, weights = weights)
}
#' @describeIn graph_measures Calculates the modularity of the graph contingent on a provided node grouping
#' @param group The node grouping to calculate the modularity on
#' @importFrom igraph modularity
#' @export
graph_modularity <- function(group, weights = NULL) {
  graph <- .G()
  group <- enquo(group)
  weights <- enquo(weights)
  group <- eval_tidy(group, .N(focused = FALSE))
  weights <- eval_tidy(weights, .E(focused = FALSE))
  modularity(graph, group, weights)
}
#' @describeIn graph_measures Calculate the global efficiency of the graph
#' @importFrom igraph global_efficiency
#' @importFrom rlang enquo eval_tidy
#' @export
graph_efficiency <- function(weights = NULL, directed = TRUE) {
  graph <- .G()
  weights <- enquo(weights)
  weights <- eval_tidy(weights, .E(focused = FALSE)) %||% NA
  global_efficiency(graph, weights = weights, directed = directed)
}
