get_group <- function(gr, fn) {
  gr %>% mutate(group = fn) %>% pull(group)
}

get_number_of_groups <- function(graph, group_clustering_function) {
  graph %>%
    mutate(groups = group_clustering_function) %>%
    dplyr::as_tibble(what = 'vertices') %>%
    distinct() %>% dplyr::count() %>% dplyr::pull(1)
}

test_that("grouping returns integer vector", {
  gr <- create_notable('zachary')
  expect_type(get_group(gr, group_components()), 'integer')
  expect_type(get_group(gr, group_edge_betweenness()), 'integer')
  expect_type(get_group(gr, group_fast_greedy()), 'integer')
  expect_type(get_group(gr, group_infomap()), 'integer')
  expect_type(get_group(gr, group_label_prop()), 'integer')
  expect_type(get_group(gr, group_louvain()), 'integer')
  expect_type(get_group(gr, group_leiden()), 'integer')
  #expect_type(get_group(gr, group_optimal()), 'integer')
  expect_type(get_group(gr, group_spinglass()), 'integer')
  expect_type(get_group(gr, group_walktrap()), 'integer')
  expect_type(get_group(gr, group_fluid()), 'integer')
  expect_type(get_group(gr, group_color()), 'integer')
  gr1 <- activate(gr, edges)
  expect_type(get_group(gr1, group_biconnected_component()), 'integer')

  skip_on_os('windows')
  expect_type(get_group(gr, group_leading_eigen()), 'integer')
})
test_that("grouping returns integer of correct length", {
  gr <- create_notable('zachary')
  expect_length(get_group(gr, group_components()), igraph::gorder(gr))
  expect_length(get_group(gr, group_edge_betweenness()), igraph::gorder(gr))
  expect_length(get_group(gr, group_fast_greedy()), igraph::gorder(gr))
  expect_length(get_group(gr, group_infomap()), igraph::gorder(gr))
  expect_length(get_group(gr, group_label_prop()), igraph::gorder(gr))
  expect_length(get_group(gr, group_louvain()), igraph::gorder(gr))
  expect_length(get_group(gr, group_leiden()), igraph::gorder(gr))
  #expect_length(get_group(gr, group_optimal()), igraph::gorder(gr))
  expect_length(get_group(gr, group_spinglass()), igraph::gorder(gr))
  expect_length(get_group(gr, group_walktrap()), igraph::gorder(gr))
  expect_length(get_group(gr, group_fluid()), igraph::gorder(gr))
  expect_length(get_group(gr, group_color()), igraph::gorder(gr))
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
  expect_error(get_group(gr1, group_leiden()))
  #expect_error(get_group(gr1, group_optimal()))
  expect_error(get_group(gr1, group_spinglass()))
  expect_error(get_group(gr1, group_walktrap()))
  expect_error(get_group(gr1, group_fluid()))
  expect_error(get_group(gr1, group_color()))

  skip_on_os('windows')
  expect_error(get_group(gr1, group_leading_eigen()))
})

test_that("grouping with fixed number of groups", {
  gr <- create_notable('zachary')
  expect_equal(
    get_number_of_groups(gr, group_edge_betweenness(n_groups = 4)), 4
  )
  expect_equal(
    get_number_of_groups(gr, group_fast_greedy(n_groups = 4)), 4
  )
  expect_equal(
    get_number_of_groups(gr, group_leading_eigen(n_groups = 32)), 32
  )
  expect_equal(
    get_number_of_groups(gr, group_walktrap(n_groups = 7)), 7
  )
  expect_equal(
    get_number_of_groups(gr, group_fluid(n_groups = 7)), 7
  )
})

test_empty_context()
