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
