#' Calculate node pair properties
#'
#' This set of functions can be used for calculations that involve node pairs.
#' If the calculateable measure is not symmetric the function will come in two
#' flavours, differentiated with `_to`/`_from` suffix. The `*_to()` functions
#' will take the provided node indexes as the target node (recycling if
#' necessary). For the `*_from()` functions the provided nodes are taken as
#' the source. As for the other wrappers provided, they are intended
#' for use inside the `tidygraph` framework and it is thus not necessary to
#' supply the graph being computed on as the context is known.
#'
#' @return A numeric vector of the same length as the number of nodes in the
#' graph
#'
#' @name pair_measures
#' @rdname pair_measures
#'
#' @examples
#' # Calculate the distance to the center node
#' create_notable('meredith') %>%
#'   mutate(dist_to_center = node_distance_to(node_is_center()))
NULL

#' @describeIn pair_measures Calculate the adhesion to the specified node. Wraps [igraph::edge_connectivity()]
#' @export
#' @importFrom igraph edge_connectivity gorder
#'
#' @param nodes The other part of the node pair (the first part is the node
#' defined by the row). Recycled if necessary.
node_adhesion_to <- function(nodes) {
  expect_nodes()
  graph <- .G()
  nodes <- as_ind(nodes, gorder(graph))
  source <- seq_len(gorder(graph))
  target <- rep(nodes, length.out = length(source))
  adhesion <- Map(function(s, t) {
    if (s == t) return(NA)
    edge_connectivity(graph, source = s, target = t, checks = TRUE)
  }, s = source, t = target)
  unlist(adhesion)
}

#' @describeIn pair_measures Calculate the adhesion from the specified node. Wraps [igraph::edge_connectivity()]
#' @export
#' @importFrom igraph edge_connectivity gorder
node_adhesion_from <- function(nodes) {
  expect_nodes()
  graph <- .G()
  nodes <- as_ind(nodes, gorder(graph))
  target <- seq_len(gorder(graph))
  source <- rep(nodes, length.out = length(target))
  adhesion <- Map(function(s, t) {
    if (s == t) return(NA)
    edge_connectivity(graph, source = s, target = t, checks = TRUE)
  }, s = source, t = target)
  unlist(adhesion)
}

#' @describeIn pair_measures Calculate the cohesion to the specified node. Wraps [igraph::vertex_connectivity()]
#' @export
#' @importFrom igraph vertex_connectivity gorder
node_cohesion_to <- function(nodes) {
  expect_nodes()
  graph <- .G()
  nodes <- as_ind(nodes, gorder(graph))
  source <- seq_len(gorder(graph))
  target <- rep(nodes, length.out = length(source))
  neigh <- lapply(ego(graph, 1, source, 'out', mindist = 1), as.integer)
  adhesion <- Map(function(s, t) {
    if (s == t) return(NA)
    if (t %in% neigh[[s]]) return(NA)
    vertex_connectivity(graph, source = s, target = t, checks = TRUE)
  }, s = source, t = target)
  unlist(adhesion)
}

#' @describeIn pair_measures Calculate the cohesion from the specified node. Wraps [igraph::vertex_connectivity()]
#' @export
#' @importFrom igraph vertex_connectivity gorder ego
node_cohesion_from <- function(nodes) {
  expect_nodes()
  graph <- .G()
  nodes <- as_ind(nodes, gorder(graph))
  target <- seq_len(gorder(graph))
  source <- rep(nodes, length.out = length(target))
  neigh <- lapply(ego(graph, 1, source, 'out', mindist = 1), as.integer)
  adhesion <- Map(function(s, t) {
    if (s == t) return(NA)
    if (t %in% neigh[[s]]) return(NA)
    vertex_connectivity(graph, source = s, target = t, checks = TRUE)
  }, s = source, t = target)
  unlist(adhesion)
}

#' @describeIn pair_measures Calculate various distance metrics between node pairs. Wraps [igraph::distances()]
#' @export
#' @importFrom igraph distances gorder
#'
#' @param mode How should edges be followed? If `'all'` all edges are
#' considered, if `'in'` only inbound edges are considered, and if `'out'` only
#' outbound edges are considered
#' @param weights The weights to use for calculation
#' @param algorithm The distance algorithms to use. By default it will try to
#' select the fastest suitable algorithm. Possible values are `"automatic"`,
#' `"unweighted"`, `"dijkstra"`, `"bellman-ford"`, and `"johnson"`
node_distance_to <- function(nodes, mode = 'out', weights = NA, algorithm = 'automatic') {
  expect_nodes()
  graph <- .G()
  weights <- enquo(weights)
  weights <- eval_tidy(weights, .E())
  nodes <- as_ind(nodes, gorder(graph))
  source <- seq_len(gorder(graph))
  target <- rep(nodes, length.out = length(source))
  target_unique <- unique(target)
  dist <- distances(graph, v = source, to = target_unique, mode = mode, weights = weights, algorithm = algorithm)
  unlist(Map(function(s, t) {dist[s, t]}, s = source, t = match(target, target_unique)))
}

#' @describeIn pair_measures Calculate various distance metrics between node pairs. Wraps [igraph::distances()]
#' @export
#' @importFrom igraph distances gorder
node_distance_from <- function(nodes, mode = 'out', weights = NA, algorithm = 'automatic') {
  expect_nodes()
  graph <- .G()
  weights <- enquo(weights)
  weights <- eval_tidy(weights, .E())
  nodes <- as_ind(nodes, gorder(graph))
  target <- seq_len(gorder(graph))
  source <- rep(nodes, length.out = length(target))
  source_unique <- unique(source)
  dist <- distances(graph, v = source_unique, to = target, mode = mode, weights = weights, algorithm = algorithm)
  unlist(Map(function(s, t) {dist[s, t]}, s = match(source, source_unique), t = target))
}

#' @describeIn pair_measures Calculate node pair cocitation count. Wraps [igraph::cocitation()]
#' @export
#' @importFrom igraph cocitation gorder
node_cocitation_with <- function(nodes) {
  expect_nodes()
  graph <- .G()
  nodes <- as_ind(nodes, gorder(graph))
  source <- seq_len(gorder(graph))
  target <- rep(nodes, length.out = length(source))
  cocite <- cocitation(graph)
  unlist(Map(function(s, t) {cocite[s, t]}, s = source, t = target))
}

#' @describeIn pair_measures Calculate node pair bibliographic coupling. Wraps [igraph::bibcoupling()]
#' @export
#' @importFrom igraph bibcoupling gorder
node_bibcoupling_with <- function(nodes) {
  expect_nodes()
  graph <- .G()
  nodes <- as_ind(nodes, gorder(graph))
  source <- seq_len(gorder(graph))
  target <- rep(nodes, length.out = length(source))
  bibc <- bibcoupling(graph)
  unlist(Map(function(s, t) {bibc[s, t]}, s = source, t = target))
}

#' @describeIn pair_measures Calculate various node pair similarity measures. Wraps [igraph::similarity()]
#' @export
#' @importFrom igraph similarity gorder
#'
#' @param loops Should loop edges be considered
#' @param method The similarity measure to calculate. Possible values are:
#' `"jaccard"`, `"dice"`, and `"invlogweighted"`
node_similarity_with <- function(nodes, mode = 'out', loops = FALSE, method = 'jaccard') {
  expect_nodes()
  graph <- .G()
  nodes <- as_ind(nodes, gorder(graph))
  source <- seq_len(gorder(graph))
  target <- rep(nodes, length.out = length(source))
  sim <- similarity(graph, mode = mode, loops = loops, method = method)
  unlist(Map(function(s, t) {sim[s, t]}, s = source, t = target))
}

#' @describeIn pair_measures Calculate the maximum flow to a node. Wraps [igraph::max_flow()]
#' @export
#' @importFrom igraph max_flow gorder
#'
#' @param capacity The edge capacity to use
node_max_flow_to <- function(nodes, capacity = NULL) {
  expect_nodes()
  graph <- .G()
  capacity <- enquo(capacity)
  capacity <- eval_tidy(capacity, .E())
  nodes <- as_ind(nodes, gorder(graph))
  source <- seq_len(gorder(graph))
  target <- rep(nodes, length.out = length(source))
  flow <- Map(function(s, t) {
    if (s == t) return(NA)
    max_flow(graph, source = s, target = t, capacity = capacity)$value
  }, s = source, t = target)
  unlist(flow)
}

#' @describeIn pair_measures Calculate the maximum flow from a node. Wraps [igraph::max_flow()]
#' @export
#' @importFrom igraph max_flow gorder
node_max_flow_from <- function(nodes, capacity = NULL) {
  expect_nodes()
  graph <- .G()
  capacity <- enquo(capacity)
  capacity <- eval_tidy(capacity, .E())
  nodes <- as_ind(nodes, gorder(graph))
  target <- seq_len(gorder(graph))
  source <- rep(nodes, length.out = length(target))
  flow <- Map(function(s, t) {
    if (s == t) return(NA)
    max_flow(graph, source = s, target = t, capacity = capacity)$value
  }, s = source, t = target)
  unlist(flow)
}
