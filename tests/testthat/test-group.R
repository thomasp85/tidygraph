context("group")

get_group <- function(gr, fn) {
  gr %>% mutate(group = fn) %>% pull(group)
}

get_number_of_communities <- function(graph, group_clustering_function) {
  graph %>%
    mutate(community = group_clustering_function) %>%
    dplyr::as_tibble(what='vertices') %>%
    distinct() %>% dplyr::count() %>% dplyr::first()
}

test_that("grouping returns integer vector", {
  gr <- create_notable('zachary')
  expect_is(get_group(gr, group_components()), 'integer')
  expect_is(get_group(gr, group_edge_betweenness()), 'integer')
  expect_is(get_group(gr, group_fast_greedy()), 'integer')
  expect_is(get_group(gr, group_infomap()), 'integer')
  expect_is(get_group(gr, group_label_prop()), 'integer')
  expect_is(get_group(gr, group_louvain()), 'integer')
  #expect_is(get_group(gr, group_optimal()), 'integer')
  expect_is(get_group(gr, group_spinglass()), 'integer')
  expect_is(get_group(gr, group_walktrap()), 'integer')
  gr1 <- activate(gr, edges)
  expect_is(get_group(gr1, group_biconnected_component()), 'integer')

  skip_on_os('windows')
  expect_is(get_group(gr, group_leading_eigen()), 'integer')
})
test_that("grouping returns integer of correct length", {
  gr <- create_notable('zachary')
  expect_length(get_group(gr, group_components()), igraph::gorder(gr))
  expect_length(get_group(gr, group_edge_betweenness()), igraph::gorder(gr))
  expect_length(get_group(gr, group_fast_greedy()), igraph::gorder(gr))
  expect_length(get_group(gr, group_infomap()), igraph::gorder(gr))
  expect_length(get_group(gr, group_label_prop()), igraph::gorder(gr))
  expect_length(get_group(gr, group_louvain()), igraph::gorder(gr))
  #expect_length(get_group(gr, group_optimal()), igraph::gorder(gr))
  expect_length(get_group(gr, group_spinglass()), igraph::gorder(gr))
  expect_length(get_group(gr, group_walktrap()), igraph::gorder(gr))
  gr1 <- activate(gr, edges)
  expect_length(get_group(gr1, group_biconnected_component()), igraph::gsize(gr1))

  skip_on_os('windows')
  expect_length(get_group(gr, group_leading_eigen()), igraph::gorder(gr))
})
test_that("grouping requires correct activation", {
  gr <- create_notable('zachary')
  expect_error(get_group(gr, group_biconnected_component()))
  gr1 <- activate(gr, edges)
  expect_error(get_group(gr1, group_components()))
  expect_error(get_group(gr1, group_edge_betweenness()))
  expect_error(get_group(gr1, group_fast_greedy()))
  expect_error(get_group(gr1, group_infomap()))
  expect_error(get_group(gr1, group_label_prop()))
  expect_error(get_group(gr1, group_louvain()))
  #expect_error(get_group(gr1, group_optimal()))
  expect_error(get_group(gr1, group_spinglass()))
  expect_error(get_group(gr1, group_walktrap()))

  skip_on_os('windows')
  expect_error(get_group(gr1, group_leading_eigen()))
})

test_that("clustering with fixed number of communities works", {
  gr <- create_notable('zachary')
  expect_equal(
    get_number_of_communities(gr, group_edge_betweenness(n_communities = 4)), 4
  )
  expect_equal(
    get_number_of_communities(gr, group_fast_greedy(n_communities = 4)), 4
  )
  expect_equal(
    get_number_of_communities(gr, group_leading_eigen(n_communities = 32)), 32
  )
})
