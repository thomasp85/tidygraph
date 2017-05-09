
<!-- README.md is generated from README.Rmd. Please edit that file -->
tidygraph
=========

[![Travis-CI Build Status](https://travis-ci.org/thomasp85/tidygraph.svg?branch=master)](https://travis-ci.org/thomasp85/tidygraph) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/thomasp85/tidygraph?branch=master&svg=true)](https://ci.appveyor.com/project/thomasp85/tidygraph) [![CRAN\_Release\_Badge](http://www.r-pkg.org/badges/version-ago/tidygraph)](https://CRAN.R-project.org/package=tidygraph) [![CRAN\_Download\_Badge](http://cranlogs.r-pkg.org/badges/tidygraph)](https://CRAN.R-project.org/package=tidygraph)

This package provide a tidy API to graph/network manipulation. While network data itself is not tidy, it can be envisioned as two tidy tables, one for node data and one for edge data. `tidygraph` provides a way to switch between the two tables and provide `dplyr` verbs for manipulating them. Furthermore it provides access to a lot of graph algorithms with return values that facilitate their use in a tidy workflow.

**This is a work in progress**

An example
----------

``` r
library(igraph)
library(tidygraph)

gr <- as_tbl_graph(erdos.renyi.game(10, 0.5)) %>% 
  activate(nodes) %>% 
  mutate(rand = sample(n()), even = rand %% 2 == 0) %>% 
  activate(edges) %>% 
  arrange(desc(to))
```

Roadmap
-------

### 1. Support relevant dplyr verbs

The goal is to support all verbs from dplyr that makes sense, which is almost all of them. It is definetly easier to list the ones that wont get supported and describe why:

1.  **All summarise functions:** Summarising nodes and edges in a graph context is ill-defined as it is unclear how the resulting graph should be created. A summarise operation modifies the number of rows in the data, but unlike filtering there are no specific rows that are retained. An alternative `collapse` functionality is under consideration where nodes (and edges) can be merged. If data summaries are needed these can be achieved by extracting the node or edge data using `as_tibble` prior to using `summarise` (note that this will remove the graph context)

2.  **do:** The rationale is really just like the above - `do` can potentially modify the data in ways that does not make sense in a graph context. The solution is again to extract the data prior to the `do` call.

### 2. Provide Constructors for all general relational data structures

The goal is that any data structure seems relational should be able to be fed into `as_tbl_graph` - this entails conversion functions into `igraph,` which is the underlying data structure that powers `tidygraph`. Currently the following is supported:

-   **`igraph`** --- well duh
-   **`list`** depending on the format it will either be parsed as an adjacency list or a list containing a `nodes` data frame and an `edges` data frame
-   **`data.frame`** parsed as an edgelist with additional edge attributes
-   **`matrix`** depending on the format it will either be parsed as a plain edgelist, an adjacency matrix, or an incidence matrix.

The following data structures are planned for support:

-   `stats::hclust` and `stats::dendrogram`
-   `network::network`
-   `ape::phylo`
-   `data.tree::data.tree`
-   `graph::graph` from bioconductor

Some of these might already work if they contain an `as.igraph` method as this is tried by default.

### 3. Provide verbs specific to graph analysis

As discussed above, `collapse` could be provided to combine nodes and automatically update the edges to fit, are to combine parallel edges. Another plan is to provide a `split_by` method that creates temporary sub graphs based on either edge or node attributes (kind of like `group_by` but updating the underlying graph structure as well). More ideas to come in time

### 4. Provide a tidy interface to all igraphs algorithms

All algorithms where it makes sense should get a wrapper where it is not necessary to specify the graph object and which nodes or edges are getting referenced. For example, inside a `mutate` call it should be possible to just call `degree()` and get a vector of node degrees returned in the correct order. This last point is probably where the most work is required.
