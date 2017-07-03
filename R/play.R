# Generate random graphs based on a specific graph model
#
# This set of functions can be used to simulate different types of graphs by
# randomising connections between nodes based on different underlying models.
# Sometimes this type of graph simulation is called a game, which is the reason
# for the `play_*` pronoun. All of these functions wraps different igraph
# `sample_*` functions but with a consistent argument naming scheme and always
# returning a tbl_graph. This help page is mainly intended as an overview, as
# all models are fully documented in their igraph docs that are linked to at
# each function.
#
# @param n,n1,n2 The number of nodes in the graph. For bipartite graphs `n1`
# and `n2` specifies the number of nodes of each type.
# @param p The probabilty of an edge occuring
# @param m The number of edges in the graph
# @param m_between The number of edges between groups/islands
# @param n_islands The number of densely connected islands
# @param size_islands The number of nodes in each island
# @param types The type of each node in the graph, enumerated from 0
# @param n_types The number of different node types in the graph
# @param p_type The probability that a node will be the given type. Either a
# vector or a matrix, depending on the game
# @param p_pref The probability that an edge will be made to a type. Either a
# vector or a matrix, depending on the game
# @param p_rewire The rewiring probability of edges
# @param p_forward,p_backward Forward and backward burning probability
# @param p_between,p_within The probability of edges within and between groups/blocks
# @param out_degree,in_degree The degrees of each node in the graph
# @param out_fit,in_fit The fitness of each node
# @param out_exp,in_exp Power law exponent of degree distribution
# @param growth The number of edges added at each iteration
# @param growth_dist The distribution of the number of added edges at each iteration
# @param use_out Should outbound edges be used for calculating citation probability
# @param appeal_zero The appeal value for unconnected nodes
# @param appeal_zero_age The appeal value of nodes without age
# @param coefficient The coefficient of the degree dependent part of attrictiveness
# @param coefficient_age The coefficient of the age dependent part of attrictiveness
# @param window The aging window to take into account when calculating the preferential attraction
# @param fixed Should n_types be understood as a fixed number of nodes for each
# type rather than as a probability
# @param bins The number of aging bins
# @param position The latent position of each node by column.
# @param n_dim,dim_size The dimension and size of the starting lattice
# @param order The neighborhood size to create connections from
# @param correct Use finite size correction
# @param callaway Use the callaway version of the trait based game
# @param radius The radius within which vertices are connected
# @param size_blocks The number of vertices in each block
# @param rho The fraction of vertices per cluster
# @param torus Should the vertices be distributed on a torus instead of a plane
# @param citation Should a citation graph be created
# @param method The algorithm to use for the generation
# @param directed Should the resulting graph be directed
# @param loops Are loop edges allowed
# @param multiple Are multiple edges allowed
# @param mode The flow direction of edges
#
# @return A `tbl_graph` object
#
# @rdname play_graphs
# @name play_graphs

#' Graph games based on different node types
#'
#' This set of games are build around different types of nodes and simulating
#' their interaction. The nature of their algorithm is described in
#' detail at the linked igraph documentation.
#'
#' @param n,n1,n2 The number of nodes in the graph. For bipartite graphs `n1`
#' and `n2` specifies the number of nodes of each type.
#' @inheritParams sampling_games
#' @inheritParams evolution_games
#' @param mode The flow direction of edges
#' @param types The type of each node in the graph, enumerated from 0
#' @param n_types The number of different node types in the graph
#' @param p_type The probability that a node will be the given type. Either a
#' vector or a matrix, depending on the game
#' @param p_pref The probability that an edge will be made to a type. Either a
#' vector or a matrix, depending on the game
#' @param fixed Should n_types be understood as a fixed number of nodes for each
#' type rather than as a probability
#' @param callaway Use the callaway version of the trait based game
#'
#' @return A tbl_graph object
#'
#' @rdname type_games
#' @name type_games
#' @family graph games
#'
#' @examples
#' plot(play_bipartite(20, 30, 0.4))
#'
NULL

#' @describeIn type_games Create graphs by linking nodes of different types
#' based on a defined probability. See [igraph::sample_pref()]
#' @importFrom igraph sample_pref
#' @export
play_preference <- function(n, n_types, p_type = rep(1, n_types), p_pref = matrix(1, n_types, n_types), fixed = FALSE, directed = TRUE, loops = FALSE) {
  as_tbl_graph(sample_pref(n, n_types, p_type, fixed, p_pref, directed, loops))
}
#' @describeIn type_games Create graphs by linking nodes of different types
#' based on an asymmetric probability. See [igraph::sample_asym_pref()]
#' @importFrom igraph sample_asym_pref
#' @export
play_preference_asym <- function(n, n_types, p_type = matrix(1, n_types, n_types), p_pref = matrix(1, n_types, n_types), loops = FALSE) {
  as_tbl_graph(sample_asym_pref(n, n_types, p_type, p_pref, loops))
}
#' @describeIn type_games Create bipartite graphs of fixed size and edge count
#' or probability. See [igraph::sample_bipartite()]
#' @importFrom igraph sample_bipartite
#' @export
play_bipartite <- function(n1, n2, p, m, directed = TRUE, mode = 'out') {
  type <- if (missing(p)) {
    'gnm'
  } else {
    if (!missing(m)) warning('Ignoring "m" as "p" is provided', call. = FALSE)
    'gnp'
  }
  as_tbl_graph(sample_bipartite(n1, n2, type, p, m, directed, mode))
}
#' @describeIn type_games Create graphs by evolving a graph with type based edge
#' probabilities. See [igraph::sample_traits()] and
#' [igraph::sample_traits_callaway()]
#' @importFrom igraph sample_traits_callaway sample_traits
#' @export
play_traits <- function(n, n_types, growth = 1, p_type = rep(1, n_types), p_pref = matrix(1, n_types, n_types), callaway = TRUE, directed = TRUE) {
  if (callaway) {
    as_tbl_graph(sample_traits_callaway(n, n_types, growth, p_type, p_pref, directed))
  } else {
    as_tbl_graph(sample_traits(n, n_types, growth, p_type, p_pref, directed))
  }
}
#' @describeIn type_games Create citation graphs by evolving with type based
#' linking probability. See [igraph::sample_cit_types()] and
#' [igraph::sample_cit_cit_types()]
#' @importFrom igraph sample_cit_types sample_cit_cit_types
#' @export
play_citation_type <- function(n, growth, types = rep(0, n), p_pref = rep(1, length(unique(types))), directed = TRUE) {
  if (is.matrix(p_pref)) {
    as_tbl_graph(sample_cit_cit_types(n, growth, types, p_pref, directed))
  } else {
    as_tbl_graph(sample_cit_types(n, growth, types, p_pref, directed))
  }
}

#' Graph games based on direct sampling
#'
#' This set of graph games creates graphs directly through sampling of different
#' attributes, topologies, etc. The nature of their algorithm is described in
#' detail at the linked igraph documentation.
#'
#' @param n The number of nodes in the graph.
#' @param p The probabilty of an edge occuring
#' @param m The number of edges in the graph
#' @param directed Should the resulting graph be directed
#' @param loops Are loop edges allowed
#' @param multiple Are multiple edges allowed
#' @param method The algorithm to use for the generation. Either `'simple'`,
#' `'vl'`, or `'simple.no.multiple'`
#' @param out_degree,in_degree The degrees of each node in the graph
#' @param out_fit,in_fit The fitness of each node
#' @param out_exp,in_exp Power law exponent of degree distribution
#' @param position The latent position of each node by column.
#' @param radius The radius within which vertices are connected
#' @param torus Should the vertices be distributed on a torus instead of a plane
#' @param correct Use finite size correction
#'
#' @return A tbl_graph object
#'
#' @rdname sampling_games
#' @name sampling_games
#' @family graph games
#'
#' @examples
#' plot(play_erdos_renyi(20, 0.3))
NULL

#' @describeIn sampling_games Create graphs based on the given node degrees. See
#' [igraph::sample_degseq()]
#' @importFrom igraph sample_degseq
#' @export
play_degree <- function(out_degree, in_degree = NULL, method = 'simple') {
  as_tbl_graph(sample_degseq(out_degree, in_degree, method))
}
#' @describeIn sampling_games Create graphs with link probability given by the
#' dot product of the latent position of termintating nodes. See
#' [igraph::sample_dot_product()]
#' @importFrom igraph sample_dot_product
#' @export
play_dotprod <- function(position, directed = TRUE) {
  as_tbl_graph(sample_dot_product(position, directed))
}
#' @describeIn sampling_games Create graphs where edge probabilities are
#' proportional to terminal node fitness scores. See [igraph::sample_fitness()]
#' @importFrom igraph sample_fitness
#' @export
play_fitness <- function(m, out_fit, in_fit = NULL, loops = FALSE, multiple = FALSE) {
  as_tbl_graph(sample_fitness(m, out_fit, in_fit, loops, multiple))
}
#' @describeIn sampling_games Create graphs with an expected power-law degree
#' distribution. See [igraph::sample_fitness_pl()]
#' @importFrom igraph sample_fitness_pl
#' @export
play_fitness_power <- function(n, m, out_exp, in_exp = -1, loops = FALSE, multiple = FALSE, correct = TRUE) {
  as_tbl_graph(sample_fitness_pl(n, m, out_exp, in_exp, loops, multiple, correct))
}
#' @describeIn sampling_games Create graphs with a fixed edge probability or
#' count. See [igraph::sample_gnp()] and [igraph::sample_gnm()]
#' @importFrom igraph sample_gnm sample_gnp
#' @export
play_erdos_renyi <- function(n, p, m, directed = TRUE, loops = FALSE) {
  if (missing(p)) {
    as_tbl_graph(sample_gnm(n, m, directed, loops))
  } else {
    if (!missing(m)) warning('Ignoring "m" as "p" is provided', call. = FALSE)
    as_tbl_graph(sample_gnp(n, p, directed, loops))
  }
}
#' @describeIn sampling_games Create graphs by positioning nodes on a plane or
#' torus and connecting nearby ones. See [igraph::sample_grg()]
#' @importFrom igraph sample_grg
#' @export
play_geometry <- function(n, radius, torus = FALSE) {
  as_tbl_graph(sample_grg(n, radius, torus, TRUE))
}

#' Graph games based on evolution
#'
#' This games create graphs through different types of evolutionary mechanisms
#' (not necessarily in a biological sense). The nature of their algorithm is
#' described in detail at the linked igraph documentation.
#'
#' @inheritParams sampling_games
#' @param growth The number of edges added at each iteration
#' @param growth_dist The distribution of the number of added edges at each iteration
#' @param use_out Should outbound edges be used for calculating citation probability
#' @param appeal_zero The appeal value for unconnected nodes
#' @param appeal_zero_age The appeal value of nodes without age
#' @param coefficient The coefficient of the degree dependent part of attrictiveness
#' @param coefficient_age The coefficient of the age dependent part of attrictiveness
#' @param power The power of the preferential attachment
#' @param power_age The aging exponent
#' @param window The aging window to take into account when calculating the preferential attraction
#' @param bins The number of aging bins
#' @param p_pref The probability that an edge will be made to an age bin.
#' @param p_forward,p_backward Forward and backward burning probability
#' @param citation Should a citation graph be created
#' @param method The algorithm to use for graph creation. Either `'psumtree'`,
#' `'psumtree-multiple'`, or `'bag'`
#'
#' @return A tbl_graph object
#'
#' @rdname evolution_games
#' @name evolution_games
#' @seealso [play_traits()] and [play_citation_type()] for an evolutionary
#' algorithm based on different node types
#' @family graph games
#'
#' @examples
#' plot(play_forestfire(50, 0.5))
#'
NULL

#' @describeIn evolution_games Create citation graphs based on a specific age
#' link probability. See [igraph::sample_last_cit()]
#' @importFrom igraph sample_last_cit
#' @export
play_citation_age <- function(n, growth = 1, bins = n/7100, p_pref = (1:(bins + 1))^-3, directed = TRUE) {
  as_tbl_graph(sample_last_cit(n, growth, bins, p_pref, directed))
}
#' @describeIn evolution_games Create graphs by simulating the spead of fire in
#' a forest. See [igraph::sample_forestfire()]
#' @importFrom igraph sample_forestfire
#' @export
play_forestfire <- function(n, p_forward, p_backward = p_forward, growth = 1, directed = TRUE) {
  as_tbl_graph(sample_forestfire(n, p_forward, p_forward/p_backward, growth, directed))
}
#' @describeIn evolution_games Create graphs by adding a fixed number of edges
#' at each iteration. See [igraph::sample_growing()]
#' @importFrom igraph sample_growing
#' @export
play_growing <- function(n, growth = 1, directed = TRUE, citation = FALSE) {
  as_tbl_graph(sample_growing(n, growth, directed, citation))
}
#' @describeIn evolution_games Create graphs based on the Barabasi-Alberts
#' preferential attachment model. See [igraph::sample_pa()]
#' @importFrom igraph sample_pa
#' @export
play_barabasi_albert <- function(n, power, growth = 1, growth_dist = NULL, use_out = FALSE, appeal_zero = 1, directed = TRUE, method = 'psumtree') {
  if (length(growth) == 1) {
    m <- growth
    out.seq <- NULL
  } else {
    m <- NULL
    out.seq <- growth
  }
  if (!is.null(growth_dist)) {
    m <- NULL
    out.seq <- NULL
  }
  as_tbl_graph(sample_pa(n, power, m, growth_dist, out.seq, use_out, appeal_zero, directed, method))
}
#' @describeIn evolution_games Create graphs based on the Barabasi-Alberts
#' preferential attachment model, incoorporating node age preferrence. See
#' [igraph::sample_pa_age()].
#' @importFrom igraph sample_pa_age
#' @export
play_barabasi_albert_aging <- function(n, power, power_age, growth = 1, growth_dist = NULL, bins = 300, use_out = FALSE, appeal_zero = 1, appeal_zero_age = 0, directed = TRUE, coefficient = 1, coefficient_age = 1, window = NULL) {
  if (length(growth) == 1) {
    m <- growth
    out.seq <- NULL
  } else {
    m <- NULL
    out.seq <- growth
  }
  if (!is.null(growth_dist)) {
    m <- NULL
    out.seq <- NULL
  }
  as_tbl_graph(sample_pa_age(n, power, power_age, m, bins, growth_dist, out.seq, use_out, directed, appeal_zero, appeal_zero_age, coefficient, coefficient_age, window))
}

#' Graph games based on connected components
#'
#' This set of graph creation algorithms simulate the topology by, in some way,
#' connecting subgraphs. The nature of their algorithm is described in detail at
#' the linked igraph documentation.
#'
#' @inheritParams sampling_games
#' @param m_between The number of edges between groups/islands
#' @param n_islands The number of densely connected islands
#' @param size_islands The number of nodes in each island
#' @param size_blocks The number of vertices in each block
#' @param p_between,p_within The probability of edges within and between groups/blocks
#' @param rho The fraction of vertices per cluster
#' @param n_dim,dim_size The dimension and size of the starting lattice
#' @param p_rewire The rewiring probability of edges
#' @param order The neighborhood size to create connections from
#'
#' @return A tbl_graph object
#'
#' @rdname component_games
#' @name component_games
#' @family graph games
#'
#' @examples
#' plot(play_islands(4, 10, 0.7, 3))
#'
NULL

#' @describeIn component_games Create graphs by sampling from stochastic block
#' model. See [igraph::sample_sbm()]
#' @importFrom igraph sample_sbm
#' @export
play_blocks <- function(n, size_blocks, p_between, directed = TRUE, loops = FALSE) {
  as_tbl_graph(sample_sbm(n, p_between, size_blocks, directed, loops))
}
#' @describeIn component_games Create graphs by sampling from the hierarchical
#' stochastic block model. See [igraph::sample_hierarchical_sbm()]
#' @importFrom igraph sample_hierarchical_sbm
#' @export
play_blocks_hierarchy <- function(n, size_blocks, rho, p_within, p_between) {
  as_tbl_graph(sample_hierarchical_sbm(n, size_blocks, rho, p_within, p_between))
}
#' @describeIn component_games Create graphs with fixed size and edge
#' probability of subgraphs as well as fixed edge count between subgraphs. See
#' [igraph::sample_islands()]
#' @importFrom igraph sample_islands
#' @export
play_islands <- function(n_islands, size_islands, p_within, m_between) {
  as_tbl_graph(sample_islands(n_islands, size_islands, p_within, m_between))
}
#' @describeIn component_games Create graphs based on the Watts-Strogatz small-
#' world model. See [igraph::sample_smallworld()]
#' @importFrom igraph sample_smallworld
#' @export
play_smallworld <- function(n_dim, dim_size, order, p_rewire, loops = FALSE, multiple = FALSE) {
  as_tbl_graph(sample_smallworld(n_dim, dim_size, order, p_rewire, loops, multiple))
}
