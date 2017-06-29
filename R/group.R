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
#' @param ... arguments passed on to the clustering function in question
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
group_components <- function(...) {
  expect_nodes()
  as.integer(components(graph = .G(), ...)$membership)
}
#' @describeIn group_graph Group densely connected nodes using [igraph::cluster_edge_betweenness()]
#' @importFrom igraph membership cluster_edge_betweenness
#' @export
group_edge_betweenness <- function(...) {
  expect_nodes()
  as.integer(membership(cluster_edge_betweenness(graph = .G(), ...)))
}
#' @describeIn group_graph Group nodes by optimising modularity using [igraph::cluster_fast_greedy()]
#' @importFrom igraph membership cluster_fast_greedy
#' @export
group_fast_greedy <- function(...) {
  expect_nodes()
  as.integer(membership(cluster_fast_greedy(graph = .G(), ...)))
}
#' @describeIn group_graph Group nodes by minimizing description length using [igraph::cluster_infomap()]
#' @importFrom igraph membership cluster_infomap
#' @export
group_infomap <- function(...) {
  expect_nodes()
  as.integer(membership(cluster_infomap(graph = .G(), ...)))
}
#' @describeIn group_graph Group nodes by propagating labels using [igraph::cluster_label_prop()]
#' @importFrom igraph membership cluster_label_prop
#' @export
group_label_prop <- function(...) {
  expect_nodes()
  as.integer(membership(cluster_label_prop(graph = .G(), ...)))
}
#' @describeIn group_graph Group nodes based on the leading eigenvector of the modularity matrix using [igraph::cluster_leading_eigen()]
#' @importFrom igraph membership cluster_leading_eigen
#' @export
group_leading_eigen <- function(...) {
  expect_nodes()
  as.integer(membership(cluster_leading_eigen(graph = .G(), ...)))
}
#' @describeIn group_graph Group nodes by multilevel optimisation of modularity using [igraph::cluster_louvain()]
#' @importFrom igraph membership cluster_louvain
#' @export
group_louvain <- function(...) {
  expect_nodes()
  as.integer(membership(cluster_louvain(graph = .G(), ...)))
}
#' @describeIn group_graph Group nodes by optimising the moldularity score using [igraph::cluster_optimal()]
#' @importFrom igraph membership cluster_optimal
#' @export
group_optimal <- function(...) {
  expect_nodes()
  as.integer(membership(cluster_optimal(graph = .G(), ...)))
}
#' @describeIn group_graph Group nodes using simulated annealing with [igraph::cluster_spinglass()]
#' @importFrom igraph membership cluster_spinglass
#' @export
group_spinglass <- function(...) {
  expect_nodes()
  as.integer(membership(cluster_spinglass(graph = .G(), ...)))
}
#' @describeIn group_graph Group nodes via short random walks using [igraph::cluster_walktrap()]
#' @importFrom igraph membership cluster_walktrap
#' @export
group_walktrap <- function(...) {
  expect_nodes()
  as.integer(membership(cluster_walktrap(graph = .G(), ...)))
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
