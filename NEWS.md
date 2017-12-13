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
