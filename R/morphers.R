#' Functions to generate alternate representations of graphs
#'
#' These functions are meant to be passed into [morph()] to create a temporary
#' alternate representation of the input graph. They are thus not meant to be
#' called directly. See below for detail of each morpher.
#'
#' @param graph A `tbl_graph`
#'
#' @param ... Arguments to pass on to [filter()] or [group_by()]
#'
#' @param subset_by,split_by Whether to create subgraphs based on nodes or edges
#'
#' @return A list of `tbl_graph`s
#'
#' @rdname morphers
#' @name morphers
#'
#' @examples
#' # Compute only on a subgraph of every even node
#' create_notable('meredith') %>%
#'   morph(to_subgraph, seq_len(graph_order()) %% 2 == 0) %>%
#'   mutate(neighbour_count = centrality_degree()) %>%
#'   unmorph()
NULL

#' @describeIn morphers Convert a graph to its line graph. When unmorphing node
#' data will be merged back into the original edge data. Edge data will be
#' ignored.
#' @importFrom igraph make_line_graph
#' @export
to_linegraph <- function(graph) {
  line_graph <- as_tbl_graph(make_line_graph(graph))
  line_graph <- mutate(activate(line_graph, 'nodes'), .tidygraph_edge_index = E(graph)$.tidygraph_edge_index)
  list(
    line_graph = line_graph
  )
}
#' @describeIn morphers Convert a graph to a single subgraph. `...` is evaluated
#' in the same manner as `filter`. When unmorphing all data in the subgraph
#' will get merged back.
#' @importFrom igraph induced_subgraph subgraph.edges
#' @export
to_subgraph <- function(graph, ..., subset_by = NULL) {
  if (is.null(subset_by)) {
    subset_by <- active(graph)
    message('Subsetting by ', subset_by)
  }
  ind <- as_tibble(graph, active = subset_by)
  ind <- mutate(ind, .tidygraph_index = seq_len(n()))
  ind <- filter(ind, ...)
  ind <- ind$.tidygraph_index
  subset <- switch(
    subset_by,
    nodes = induced_subgraph(graph, ind),
    edges = subgraph.edges(graph, ind)
  )
  list(
    subgraph = as_tbl_graph(subset)
  )
}
#' @describeIn morphers Convert a graph into a list of separate subgraphs. `...`
#' is evaluated in the same manner as `group_by`. When unmorphing all data in
#' the subgraphs will get merged back, but in the case of `split_by = 'edges'`
#' only the first instance of node data will be used (as the same node can be
#' present in multiple subgraphs).
#' @importFrom igraph induced_subgraph subgraph.edges
#' @importFrom stats setNames
#' @export
to_split <- function(graph, ..., split_by = NULL) {
  if (is.null(split_by)) {
    split_by <- active(graph)
    message('Subsetting by ', split_by)
  }
  ind <- as_tibble(graph, active = split_by)
  ind <- group_by(ind, ...)
  splits <- lapply(attr(ind, 'indices'), function(i) {
    g <- switch(
      split_by,
      nodes = induced_subgraph(graph, i+1),
      edges = subgraph.edges(graph, i+1)
    )
    as_tbl_graph(g)
  })
  split_names <- attr(ind, 'labels')
  split_names <- lapply(names(split_names), function(n) {
    paste(n, split_names[[n]], sep = ': ')
  })
  split_names <- do.call(paste, modifyList(unname(split_names), list(sep = ', ')))
  setNames(splits, split_names)
}
#' @describeIn morphers Split a graph into its separate components. When
#' unmorphing all data in the subgraphs will get merged back.
#' @param type The type of component to split into. Either `'weak'` or `'strong'`
#' @importFrom igraph decompose
#' @export
to_components <- function(graph, type = 'weak') {
  graphs <- decompose(graph, mode= type)
  graphs <- lapply(graphs, as_tbl_graph)
  graphs
}
#' @describeIn morphers Convert a graph into its complement. When unmorphing
#' only node data will get merged back.
#' @param loops Should loops be included. Defaults to `FALSE`
#' @importFrom igraph complementer
#' @export
to_complement <- function(graph, loops = FALSE) {
  complement <- complementer(graph, loops = loops)
  list(
    complement = as_tbl_graph(complement)
  )
}
#' @describeIn morphers Convert a graph into the local neighborhood around a
#' single node. When unmorphing all data will be merged back.
#' @param node The center of the neighborhood
#' @param order The radius of the neighborhood
#' @param mode How should edges be followed? `'out'` only follows outbound
#' edges, `'in'` only follows inbound edges, and `'all'` follows all edges. This
#' parameter is ignored for undirected graphs.
#' @importFrom igraph make_ego_graph gorder
#' @export
to_local_neighborhood <- function(graph, node, order = 1, mode = 'all') {
  node <- as_ind(node, gorder(graph))
  ego <- make_ego_graph(graph, order = order, nodes = node, mode = mode)
  list(
    neighborhood = as_tbl_graph(ego[[1]])
  )
}
#' @describeIn morphers Convert a graph into its dominator tree based on a
#' specific root. When unmorphing only node data will get merged back.
#' @param root The root of the tree
#' @importFrom igraph dominator_tree gorder
#' @export
to_dominator_tree <- function(graph, root, mode = 'out') {
  root <- as_ind(root, gorder(graph))
  dom <- dominator_tree(graph, root = root, mode = mode)
  list(
    dominator_tree = as_tbl_graph(dom$domtree)
  )
}
#' @describeIn morphers Convert a graph into its minimum spanning tree/forest.
#' When unmorphing all data will get merged back.
#' @param weights Optional edge weights for the calculations
#' @importFrom igraph mst
#' @importFrom rlang enquo eval_tidy
#' @export
to_minimum_spanning_tree <- function(graph, weights = NULL) {
  weights <- enquo(weights)
  weights <- eval_tidy(weights, as_tibble(graph, active = 'edges'))
  mst <- mst(graph, weights = weights)
  list(
    mst = as_tbl_graph(mst)
  )
}
#' @describeIn morphers Limit a graph to the shortest path between two nodes.
#' When unmorphing all data is merged back.
#' @param from,to The start and end node of the path
#' @importFrom igraph shortest_paths gorder
#' @importFrom rlang enquo eval_tidy
#' @export
to_shortest_path <- function(graph, from, to, mode = 'out', weights = NULL) {
  from <- as_ind(from, gorder(graph))
  to <- as_ind(to, gorder(graph))
  weights <- enquo(weights)
  weights <- eval_tidy(weights, as_tibble(graph, active = 'edges'))
  path <- shortest_paths(graph, from = from, to = to, mode = mode, weights = weights, output = 'both')
  short_path <- slice(activate(graph, 'edges'), as.integer(path$epath[[1]]))
  short_path <- slice(activate(short_path, 'nodes'), as.integer(path$vpath[[1]]))
  list(
    shortest_path = short_path
  )
}
#' @describeIn morphers Convert a graph into a breath-first search tree based on
#' a specific root. When unmorphing only node data is merged back.
#' @param unreachable Should the search jump to a node in a new component when
#' stuck.
#' @importFrom igraph bfs gorder
#' @export
to_bfs_tree <- function(graph, root, mode = 'out', unreachable = FALSE) {
  root <- as_ind(root, gorder(graph))
  search <- bfs(graph, root, neimode = mode, unreachable = unreachable, father = TRUE)
  bfs_graph <- search_to_graph(graph, search)
  list(
    bfs = bfs_graph
  )
}
#' @describeIn morphers Convert a graph into a depth-first search tree based on
#' a specific root. When unmorphing only node data is merged back.
#' @importFrom igraph bfs gorder
#' @export
to_dfs_tree <- function(graph, root, mode = 'out', unreachable = FALSE) {
  root <- as_ind(root, gorder(graph))
  search <- dfs(graph, root, neimode = mode, unreachable = unreachable, father = TRUE)
  dfs_graph <- search_to_graph(graph, search)
  list(
    dfs = dfs_graph
  )
}
#' @describeIn morphers Collapse parallel edges and remove loops in a graph.
#' When unmorphing all data will get merged back
#' @importFrom igraph simplify
#' @export
to_simple <- function(graph) {
  edges <- as_tibble(graph, active = 'edges')
  graph <- set_edge_attributes(graph, edges[, '.tidygraph_edge_index', drop = FALSE])
  edges$.tidygraph_edge_index <- NULL
  simple <- as_tbl_graph(simplify(graph, remove.multiple = TRUE, remove.loops = TRUE, edge.attr.comb = list))
  new_edges <- as_tibble(simple, active = 'edges')
  new_edges$.data <- lapply(new_edges$.tidygraph_edge_index, function(i) edges[i, , drop = FALSE])
  simple <- set_edge_attributes(simple, new_edges)
  list(
    simple = simple
  )
}
#' @describeIn morphers Combine multiple nodes into one. `...`
#' is evaluated in the same manner as `group_by`. When unmorphing all
#' data will get merged back.
#' @param simplify Should edges in the contracted graph be simplified? Defaults
#' to `TRUE`
#' @importFrom tidyr nest
#' @importFrom igraph contract
#' @export
to_contracted <- function(graph, ..., simplify = TRUE) {
  nodes <- as_tibble(graph, active = 'nodes')
  nodes <- group_by(nodes, ...)
  ind <- attr(nodes, 'indices')
  ind <- rep(seq_along(ind), lengths(ind))[order(unlist(ind))]
  contracted <- as_tbl_graph(contract(graph, ind, vertex.attr.comb = 'ignore'))
  nodes <- nest(nodes, .key = '.data')
  ind <- lapply(nodes$.data, `[[`, '.tidygraph_node_index')
  nodes$.data <- lapply(nodes$.data, function(x) {x$.tidygraph_node_index <- NULL; x})
  nodes$.tidygraph_node_index <- ind
  contracted <- set_node_attributes(contracted, nodes)
  if (simplify) {
    contracted <- to_simple(contracted)[[1]]
  }
  list(
    contracted = contracted
  )
}

# HELPERS -----------------------------------------------------------------

search_to_graph <- function(graph, search) {
  nodes <- as_tibble(graph, active = 'nodes')
  edges <- tibble(from = search$father, to = seq_len(nrow(nodes)))
  edges <- edges[!is.na(edges$from), , drop = FALSE]
  tbl_graph(nodes, edges)
}
