get_val <- function(gr, fn) {
  gr %>% mutate(val = fn) %>% pull(val)
}
test_that("Node measures return corrent type", {
  tree <- create_tree(10, 2) %>%
    activate(edges) %>%
    mutate(w = seq_len(n())) %>%
    activate(nodes)
  gr <- create_notable('meredith') %>%
    activate(edges) %>%
    mutate(w = seq_len(n())) %>%
    activate(nodes)
  gr_u <- convert(gr, to_undirected)
  expect_type(get_val(gr, node_constraint()), 'double')
  expect_type(get_val(gr, node_coreness()), 'double')
  expect_type(get_val(gr_u, node_diversity(w)), 'double')
  expect_type(get_val(gr_u, node_efficiency()), 'double')
  expect_type(get_val(tree, node_dominator(node_is_root())), 'double')
  expect_type(get_val(gr, node_eccentricity()), 'double')
  expect_type(get_val(tree, node_topo_order()), 'integer')
})
test_that("Node measures return correct length", {
  tree <- create_tree(10, 2) %>%
    activate(edges) %>%
    mutate(w = seq_len(n())) %>%
    activate(nodes)
  gr <- create_notable('meredith') %>%
    activate(edges) %>%
    mutate(w = seq_len(n())) %>%
    activate(nodes)
  gr_u <- convert(gr, to_undirected)
  expect_length(get_val(gr, node_constraint()), igraph::gorder(gr))
  expect_length(get_val(gr, node_coreness()), igraph::gorder(gr))
  expect_length(get_val(gr_u, node_diversity(w)), igraph::gorder(gr))
  expect_length(get_val(gr_u, node_efficiency()), igraph::gorder(gr))
  expect_length(get_val(tree, node_dominator(node_is_root())), igraph::gorder(tree))
  expect_length(get_val(gr, node_eccentricity()), igraph::gorder(gr))
  expect_length(get_val(tree, node_topo_order()), igraph::gorder(tree))
})
test_that("Node measures return correct length for focus", {
  tree <- create_tree(10, 2) %>%
    activate(edges) %>%
    mutate(w = seq_len(n())) %>%
    activate(nodes) |>
    focus(dplyr::row_number() < 3)
  gr <- create_notable('meredith') %>%
    activate(edges) %>%
    mutate(w = seq_len(n())) %>%
    activate(nodes) |>
    focus(dplyr::row_number() < 3)
  gr_u <- convert(gr, to_undirected) |>
    focus(dplyr::row_number() < 3)
  expect_length(get_val(gr, node_constraint()), 2)
  expect_length(get_val(gr, node_coreness()), 2)
  expect_length(get_val(gr_u, node_diversity(w)), 2)
  expect_length(get_val(gr_u, node_efficiency()), 2)
  expect_length(get_val(tree, node_dominator(node_is_root())), 2)
  expect_length(get_val(gr, node_eccentricity()), 2)
  expect_length(get_val(tree, node_topo_order()), 2)
})
test_that("Node measures requires active nodes", {
  tree <- create_tree(10, 2) %>%
    activate(edges) %>%
    mutate(w = seq_len(n()))
  gr <- create_notable('meredith') %>%
    activate(edges) %>%
    mutate(w = seq_len(n()))
  gr_u <- convert(gr, to_undirected)
  expect_error(get_val(gr, node_constraint()))
  expect_error(get_val(gr, node_coreness()))
  expect_error(get_val(gr_u, node_diversity(w)))
  expect_error(get_val(gr_u, node_efficiency()))
  expect_error(get_val(tree, node_dominator(node_is_root())))
  expect_error(get_val(gr, node_eccentricity()))
  expect_error(get_val(tree, node_topo_order()))
})

test_empty_context()
