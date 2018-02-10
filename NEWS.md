# tidygraph 1.0.0.9999

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
