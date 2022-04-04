context("node_measures")

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
  expect_is(get_val(gr, node_constraint()), 'numeric')
  expect_is(get_val(gr, node_coreness()), 'numeric')
  expect_is(get_val(gr_u, node_diversity(w)), 'numeric')
  expect_is(get_val(tree, node_dominator(node_is_root())), 'numeric')
  expect_is(get_val(gr, node_eccentricity()), 'numeric')
  expect_is(get_val(tree, node_topo_order()), 'integer')
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
  expect_length(get_val(tree, node_dominator(node_is_root())), igraph::gorder(tree))
  expect_length(get_val(gr, node_eccentricity()), igraph::gorder(gr))
  expect_length(get_val(tree, node_topo_order()), igraph::gorder(tree))
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
  expect_error(get_val(tree, node_dominator(node_is_root())))
  expect_error(get_val(gr, node_eccentricity()))
  expect_error(get_val(tree, node_topo_order()))
})
