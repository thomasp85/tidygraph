#' Create different types of well-defined graphs
#'
#' These functions creates a long list of different types of well-defined graphs,
#' that is, their structure is not based on any randomisation. All of these
#' functions are shallow wrappers around a range of `igraph::make_*` functions
#' but returns `tbl_graph` rather than `igraph` objects.
#'
#' @param n,n1,n2 The number of nodes in the graph
#' @param directed Should the graph be directed
#' @param mode In case of a directed, non-mutual, graph should the edges flow
#' `'out'` or `'in'`
#' @param mutual Should mutual edges be created in case of the graph being
#' directed
#' @param name The name of a notable graph. See a complete list in [igraph::make_graph()]
#' @param w A matrix specifying the additional edges in the chordan ring. See
#' [igraph::make_chordal_ring()]
#' @param alphabet_size The number of unique letters in the alphabet used for
#' the graph
#' @param label_size The number of characters in each node
#' @param dim The dimensions of the lattice
#' @param circular Should each dimension in the lattice wrap around
#' @param children The number of children each node has in the tree (if possible)
#'
#' @return A tbl_graph
#'
#' @name create_graphs
#' @rdname create_graphs
#'
#' @examples
#' # Create a complete graph with 10 nodes
#' create_complete(10)
#'
NULL

#' @describeIn create_graphs Create a simple ring graph
#' @importFrom igraph make_ring
#' @export
create_ring <- function(n, directed = FALSE, mutual = FALSE) {
  as_tbl_graph(make_ring(n, directed, mutual, circular = TRUE))
}
#' @describeIn create_graphs Create a simple path
#' @importFrom igraph make_ring
#' @export
create_path <- function(n, directed = FALSE, mutual = FALSE) {
  as_tbl_graph(make_ring(n, directed, mutual, circular = FALSE))
}
#' @describeIn create_graphs Create a chordal ring
#' @importFrom igraph make_chordal_ring
#' @export
create_chordal_ring <- function(n, w) {
  as_tbl_graph(make_chordal_ring(n, w))
}
#' @describeIn create_graphs Create a de Bruijn graph with the specified alphabet and label size
#' @importFrom igraph make_de_bruijn_graph
#' @export
create_de_bruijn <- function(alphabet_size, label_size) {
  as_tbl_graph(make_de_bruijn_graph(alphabet_size, label_size))
}
#' @describeIn create_graphs Create a graph with no edges
#' @importFrom igraph make_empty_graph
#' @export
create_empty <- function(n, directed = FALSE) {
  as_tbl_graph(make_empty_graph(n, directed))
}
#' @describeIn create_graphs Create a full bipartite graph
#' @importFrom igraph make_full_bipartite_graph
#' @export
create_bipartite <- function(n1, n2, directed = FALSE, mode = 'out') {
  as_tbl_graph(make_full_bipartite_graph(n1, n2, directed, mode))
}
#' @describeIn create_graphs Create a full citation graph
#' @importFrom igraph make_full_citation_graph
#' @export
create_citation <- function(n) {
  as_tbl_graph(make_full_citation_graph(n))
}
#' @describeIn create_graphs Create a complete graph (a graph where all nodes are connected)
#' @importFrom igraph make_full_graph
#' @export
create_complete <- function(n) {
  as_tbl_graph(make_full_graph(n))
}
#' @describeIn create_graphs Create a graph based on its name. See [igraph::make_graph()]
#' @importFrom igraph make_graph
#' @export
create_notable <- function(name) {
  if (!is.character(name)) cli::cli_abort('{.arg name} must be a scalar {.cls character} vector')
  as_tbl_graph(make_graph(name))
}
#' @describeIn create_graphs Create a Kautz graph with the specified alphabet and label size
#' @importFrom igraph make_kautz_graph
#' @export
create_kautz <- function(alphabet_size, label_size) {
  as_tbl_graph(make_kautz_graph(alphabet_size, label_size))
}
#' @describeIn create_graphs Create a multidimensional grid of nodes
#' @importFrom igraph make_lattice
#' @export
create_lattice <- function(dim, directed = FALSE, mutual = FALSE, circular = FALSE) {
  as_tbl_graph(make_lattice(dim, directed = directed, mutual = mutual, circular = circular))
}
#' @describeIn create_graphs Create a star graph (A single node in the center connected to all other nodes)
#' @importFrom igraph make_star
#' @export
create_star <- function(n, directed = FALSE, mutual = FALSE, mode = 'out') {
  if (!directed) mode <- 'undirected'
  else if (mutual) mode <- 'mutual'
  as_tbl_graph(make_star(n, mode))
}
#' @describeIn create_graphs Create a tree graph
#' @importFrom igraph make_tree
#' @export
create_tree <- function(n, children, directed = TRUE, mode = 'out') {
  if (!directed) mode <- 'undirected'
  as_tbl_graph(make_tree(n, children, mode))
}
