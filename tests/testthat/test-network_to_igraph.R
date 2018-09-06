context("network_to_igraph")

build_test_graph <- function(ig_or_nw, bipartite = FALSE, seed = 1234) {
  n_nodes = 30L
  n_edges = 500L
  set.seed(seed)
  graph_attrs <- list(graph_chr = "Much Graph. Many Attributes",
                      graph_lgl = TRUE,
                      graph_int = 1L,
                      graph_dbl = 3.14)
  vertices_df <- data.frame(vertex.names = paste0("node_", seq_len(n_nodes)),
                            node_chr = sample(letters, n_nodes, replace = TRUE),
                            node_int = sample(seq.int(100L), n_nodes),
                            node_dbl = runif(n_nodes, 0, 100),
                            node_lgl = sample(c(TRUE, FALSE), n_nodes, replace = TRUE),
                            stringsAsFactors = FALSE)
  edges_df <- data.frame(from = sample(seq_len(n_nodes), n_edges, replace = TRUE),
                         to = sample(seq_len(n_nodes), n_edges, replace = TRUE),
                         edge_chr = sample(letters, n_edges, replace = TRUE),
                         edge_int = sample(seq.int(1000L), n_edges, replace = FALSE),
                         edge_dbl = runif(n_edges, 0, 1000),
                         edge_lgl = sample(c(TRUE, FALSE), n_edges, replace = TRUE),
                         stringsAsFactors = FALSE)
  if(!bipartite) {
    if(ig_or_nw == "ig") {
      names(vertices_df)[names(vertices_df) == "vertex.names"] <- "name"
      edges_df$from <- vertices_df$name[edges_df$from]
      edges_df$to <- vertices_df$name[edges_df$to]
      out <- igraph::graph_from_data_frame(edges_df, vertices = vertices_df)
      igraph::graph_attr(out) <- graph_attrs
      return(out)
    }
    if(ig_or_nw == "nw") {
      el <- as.matrix(edges_df[, c("from", "to")])
      out <- network::as.network.matrix(el, matrix.type = "edgelist", loops = TRUE,
                                        multiple = TRUE)
      for(g in names(graph_attrs)) {
        network::set.network.attribute(out, g, graph_attrs[[g]])
      }
      for(v in names(vertices_df)) {
        network::set.vertex.attribute(out, v, vertices_df[[v]])
      }
      for(e in names(edges_df)[!names(edges_df) %in% c("from", "to")]) {
        network::set.edge.attribute(out, e, edges_df[[e]])
      }
      return(out)
    }
  }
  set.seed(seed)
  affil_matrix <- matrix(rbinom((n_nodes / 2)^2, size = 1, prob = 0.5), nrow = n_nodes / 2,
                         dimnames = list(paste0("act_node_", seq_len(n_nodes / 2)),
                                         paste0("non_act_node_", seq_len(n_nodes / 2))))
  n_edges <- sum(affil_matrix)
  edges_df <- edges_df[seq_len(n_edges), 3:ncol(edges_df)]
  if(ig_or_nw == "ig") {
    names(vertices_df)[names(vertices_df) == "vertex.names"] <- "name"
    out <- igraph::graph_from_incidence_matrix(affil_matrix, directed = FALSE) 
    igraph::graph_attr(out) <- graph_attrs
    igraph::edge_attr(out) <- edges_df
    igraph::vertex_attr(out) <- vertices_df
    igraph::V(out)$type <- !igraph::bipartite.mapping(out)$type
   return(out)
  }
  if(ig_or_nw == "nw") {
    out <- network::as.network.matrix(affil_matrix, bipartite = TRUE)
    for(g in names(graph_attrs)) {
      network::set.network.attribute(out, g, graph_attrs[[g]])
    }
    for(v in names(vertices_df)) {
      network::set.vertex.attribute(out, v, vertices_df[[v]])
    }
    for(e in names(edges_df)[!names(edges_df) %in% c("from", "to")]) {
      network::set.edge.attribute(out, e, edges_df[[e]])
    }
    return(out)
  }
}

are_same_igraphs <- function(x, y) {
  get_node_attrs <- function(ig) {
    v <- igraph::as_data_frame(ig, what = "vertices")
    v[sort(names(v))]
  }
  get_edge_attrs <- function(ig) {
    e <- igraph::as_data_frame(ig)
    e[sort(names(e))]
  }
  get_graph_attrs <- function(ig) {
    g <- igraph::graph_attr(ig)
    as.data.frame(g[sort(names(g))])
  }
  all(get_node_attrs(x) == get_node_attrs(y),
      get_edge_attrs(x) == get_edge_attrs(y),
      get_graph_attrs(x) == get_graph_attrs(y))
}

test_that("unipartite network objects convert correctly", {
  expect_true(
    are_same_igraphs(network_to_igraph(build_test_graph("nw")),
                     build_test_graph("ig"))
    )
  })

test_that("bipartite network objects convert correctly", {
  expect_true(
    are_same_igraphs(network_to_igraph(build_test_graph("nw", bipartite = TRUE)),
                     build_test_graph("ig", bipartite = TRUE))
    )
  })
