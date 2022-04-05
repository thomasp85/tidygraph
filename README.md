
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidygraph <img src="man/figures/logo.png" align="right" width="140px"/>

<!-- badges: start -->

[![R build
status](https://github.com/thomasp85/tidygraph/workflows/R-CMD-check/badge.svg)](https://github.com/thomasp85/tidygraph/actions)
[![CRAN_Release_Badge](http://www.r-pkg.org/badges/version-ago/tidygraph)](https://CRAN.R-project.org/package=tidygraph)
[![CRAN_Download_Badge](http://cranlogs.r-pkg.org/badges/tidygraph)](https://CRAN.R-project.org/package=tidygraph)
[![Coverage
Status](https://img.shields.io/codecov/c/github/thomasp85/tidygraph/master.svg)](https://codecov.io/github/thomasp85/tidygraph?branch=master)
<!-- badges: end -->

This package provides a tidy API for graph/network manipulation. While
network data itself is not tidy, it can be envisioned as two tidy
tables, one for node data and one for edge data. `tidygraph` provides a
way to switch between the two tables and provides `dplyr` verbs for
manipulating them. Furthermore it provides access to a lot of graph
algorithms with return values that facilitate their use in a tidy
workflow.

## An example

``` r
library(tidygraph)

play_erdos_renyi(10, 0.5) %>% 
  activate(nodes) %>% 
  mutate(degree = centrality_degree()) %>% 
  activate(edges) %>% 
  mutate(centrality = centrality_edge_betweenness()) %>% 
  arrange(centrality)
#> # A tbl_graph: 10 nodes and 44 edges
#> #
#> # A directed simple graph with 1 component
#> #
#> # Edge Data: 44 × 3 (active)
#>    from    to centrality
#>   <int> <int>      <dbl>
#> 1     7     8       1.58
#> 2     9     7       1.7 
#> 3     9     1       1.78
#> 4     7     6       1.83
#> 5     7     5       2   
#> 6     8     5       2   
#> # … with 38 more rows
#> #
#> # Node Data: 10 × 1
#>   degree
#>    <dbl>
#> 1      2
#> 2      3
#> 3      5
#> # … with 7 more rows
```

## Overview

`tidygraph` is a huge package that exports 280 different functions and
methods. It more or less wraps the full functionality of `igraph` in a
tidy API giving you access to almost all of the `dplyr` verbs plus a few
more, developed for use with relational data.

### More verbs

`tidygraph` adds some extra verbs for specific use in network analysis
and manipulation. The `activate()` function defines whether one is
manipulating node or edge data at the moment as shown in the example
above. `bind_edges()`, `bind_nodes()`, and `bind_graphs()` let you
expand the graph structure you’re working with, while `graph_join()`
lets you merge two graphs on some node identifier. `reroute()`, on the
other hand, lets you change the terminal nodes of the edges in the
graph.

### More algorithms

`tidygraph` wraps almost all of the graph algorithms from `igraph` and
provides a consistent interface and output that always matches the
sequence of nodes and edges. All `tidygraph` algorithm wrappers are
intended for use inside verbs where they know the context they are being
called in. In the example above it is not necessary to supply the graph
nor the node/edge IDs to `centrality_degree()` and
`centrality_edge_betweenness()` as they are aware of them already. This
leads to much clearer code and less typing.

### More maps

`tidygraph` goes beyond `dplyr` and also implements graph centric
version of the `purrr` map functions. You can now call a function on the
nodes in the order of a breadth or depth first search while getting
access to the result of the previous calls.

### More morphs

`tidygraph` lets you temporarily change the representation of your
graph, do some manipulation of the node and edge data, and then change
back to the original graph with the changes being merged in
automatically. This is powered by the new `morph()`/`unmorph()` verbs
that let you e.g. contract nodes, work on the linegraph representation,
split communities to separate graphs etc. If you wish to continue with
the morphed version, the `crystallise()` verb lets you *freeze* the
temporary representation into a proper `tbl_graph`.

### More data structure support

While `tidygraph` is powered by igraph underneath it wants everyone to
join the fun. The `as_tbl_graph()` function can easily convert
relational data from all your favourite objects, such as `network`,
`phylo`, `dendrogram`, `data.tree`, `graph`, etc. More conversion will
be added in the order I become aware of them.

## Visualisation

`tidygraph` itself does not provide any means of visualisation, but it
works flawlessly with `ggraph`. This division makes it easy to develop
the visualisation and manipulation code at different speeds depending on
where the needs arise.

## Installation

`tidygraph` is available on CRAN and can be installed simply, using
`install.packages('tidygraph')`. For the development version available
on GitHub, use the `devtools` package for installation:

``` r
# install.packages('devtools')
devtools::install_github('thomasp85/tidygraph')
```

## Thanks

`tidygraph` stands on the shoulders of particularly the `igraph` and
`dplyr`/`tidyverse` teams. It would not have happened without them, so
thanks so much to them.

## Code of Conduct

Please note that the tidygraph project is released with a [Contributor
Code of
Conduct](https://tidygraph.data-imaginist.com/CODE_OF_CONDUCT.html). By
contributing to this project, you agree to abide by its terms.
