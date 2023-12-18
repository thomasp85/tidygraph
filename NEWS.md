# tidygraph 1.3.0

* Add `resolution` argument to `group_louvrain()` to mirror the igraph function
* `as_tbl_graph()` on an edge dataframe now only adds a name node attribute if 
  the edges are encoded as a character (#147)
* Added `node_is_connected()` to test whether a node is connected to a set of 
  nodes (#165)
* Deprecated `play_erdos_renyi()` in favour of `play_gnm()` and `play_gnp()` 
  (#152)
* Added the whole family of `slice_*()` functions from dplyr (#128)
* Added methods for `tidyr::replace_na()` and `tidyr::drop_na()` (#114)
* Added `edge_is_bridge()` for querying whether an edge is a bridge edge (#113)
* Added a `glimpse()` method for `tbl_graph` and `morphed_tbl_graph` objects 
  (#30)
* Add `iterate_n()` and `iterate_while()` to perform repeated modifications of
  a graph for a specific number of times or until a condition no longer is met 
  (#43)
* Add `focus()`/`unfocus()` verbs to limit node and edge algorithms to a subset
  while still keeping the full graph context (#18)
* Data frame subclasses with sticky columns (such as those from sf and tsibble)
  now works with the tbl_graph constructors (#184)
* `graph_automorphisms()` gains a `color` argument in line with capabilities in
  igraph
* `graph_mean_dist()` now supports edge weights through a new `weights` argument
* Added `to_largest_component()` morpher
* Added `graph_is_eulerian()` and `edge_rank_eulerian()` for eulerian path 
  calculations
* Added `to_random_spanning_tree()` morpher
* Added `min_order` argument to `to_components()` morpher
* Added `random_walk_rank()` to perform random walks on the graph
* Added `centrality_harmonic()` + deprecated `centrality_closeness_harmonic()`.
  The latter is an interface to netrankr while the former is a more efficient 
  and flexible igraph implementation.
* Added `group_color()` as an interface to `greedy_vertex_coloring()` in igraph
* Added `group_leiden()` to interface with `cluster_leiden()` in igraph
* Added `group_fluid()` to interface with `cluster_fluid_communities()` in igraph
* Added `edge_is_feedback_arc()` to interface with `feedback_arc_set()` in igraph
* Added `graph_efficiency()` and `node_effeciency()` interfacing with 
  `global_efficiency()` and `local_efficiency()` in igraph

# tidygraph 1.2.3

* Small updates to work with new versions of igraph and dplyr

# tidygraph 1.2.2

* Activating a grouped tbl_graph with what is already active will no longer 
  cause grouping to be dropped (#121)
* General upkeep

# tidygraph 1.2.1

* Move compiled code to cpp11
* Improve messaging with rlang and cli
* New feature: the following  hierarchical clustering functions 
  `group_edge_betweenness`, `group_fast_greedy`, `group_leading_eigen` and 
  `group_walktrap` have a new argument `n_groups` that controls the numbers of 
  groups computed. The argument expects an integer value and it is `NULL` by 
  default.

# tidygraph 1.2.0

* graph description now recognise undirected trees
* Added pkgdown site at https://tidygraph.data-imaginist.com
* Prepare tidygraph for dplyr 1.0.0 (#118 and #119)
* Add possibility of controlling which column in `nodes` are used for matching
  if the `to` and `from` columns in edges are character vectors during 
  construction (#89)
* `bind_graph()` now accepts a list of graphs as its first argument (#88)
* Add `graph_modularity()` for calculating modularity contingent on a node 
  grouping (#97)
* Edge weights are now handled more consistently to avoid igraph using a 
  possible `weight` edge attribute. `weights = NULL` will always mean that no
  edge weight is used (#106).
* Neighborhood graph in `map_local()` and siblings will now contain a 
  `.central_node` node attribute that will identify the node from which the 
  local graph has been calculated (#107)

# tidygraph 1.1.2

* Compatibility with `dplyr` 0.8

# tidygraph 1.1.1

* Better conversion of `network` objects. Old conversion could mess up edge 
  attributes.
* Changes to anticipate new version of `tibble` and `dplyr`
* `tibble`-like dimming of non-data text in printing
* Edge-length is now preserved when converting from `phylo`
* Added `to_subcomponent` morpher to work with a single component containing a 
  specified node
* Morphers that reference nodes now correctly tidy eval the node argument
* Add `node_is_adjacent` to query which nodes are directly connected to a set of
  nodes
* Add `fortify` method for `tbl_graph` object for plotting as regular data with 
  `ggplot2`

# tidygraph 1.1.0

* Fix bug when coercing to `tbl_graph` from an adjacency list containing `NULL`
  or `NA` elements.
* Change license to MIT
* Add `convert` verb to perform both `morph` and `crystallise` in one go, 
  returning a single `tbl_graph`
* When collapsing edges or nodes during `morph` the original data will be stored
  in `.orig_data` instead of `.data` to avoid conflicts with `.data` argument in
  many tidyverse verbs (**BREAKING**)
* `as_tbl_graph.data.frame` now recognises set tables (each column gives eachs
  rows membership to that set)
* Add `with_graph` to allow computation of algorithms outside of verbs
* `graph_is_*` set of querying functions has been added that all returns logical
  scalars.
* Add `%N>%` and `%E>%` for activating nodes and edges respectively as part of
  the piping.
* `mutate` now lets you reference created columns in graph algorithms so it 
  behaves in line with expected `mutate` behaviour. This has led to a slight
  performance decrease (millisecond scale). The old behaviour can be accessed
  using `mutate_as_tbl` where the graph will only get updated in the end.
* When using to_subgraph with edges, isolated nodes are no longer deleted
* `bind_graphs` now work with a single `tbl_graph`
* Added `.register_graph_context` to allow the use of tidygraph algorithms in
  external functions.
* Added `to_unfolded_tree`, `to_directed`, and `to_undirected` morphers
* Add the `node_rank_*` family of algorithms for seriation of nodes
* Added `to_hierarchical_clusters` morpher to work with hierarchical 
  representations of community detection algorithms.
* All `group_*` algorithms now ensure that the groups are enumerated in 
  descending order based on size, i.e. members of the largest group/community
  will always have `1`, etc.
* Fix a bug when filtering all nodes or edges where no nodes/edges would be 
  removed (#42)
* Added interface to `netrankr` resulting in 19 new centrality scores and a 
  manual mode for composing new centrality scores
* Added `edge_is_[from|to|between|incident]()` to help find edges related to
  certain nodes
