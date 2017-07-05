context("edge_types")

get_type <- function(gr, fn) {
  gr %>% mutate(type = fn) %>% pull(type)
}
test_that("edge types return logical", {
  gr <- create_notable('bull') %>%
    activate(edges) %>%
    bind_edges(tibble::tibble(
      to = c(1, 1, 3, 4),
      from = c(1, 3, 1, 2)
    ))
  expect_is(get_type(gr, edge_is_loop()), 'logical')
  expect_is(get_type(gr, edge_is_multiple()), 'logical')
  expect_is(get_type(gr, edge_is_mutual()), 'logical')
})
test_that("edge types return correct length", {
  gr <- create_notable('bull') %>%
    activate(edges) %>%
    bind_edges(tibble::tibble(
      to = c(1, 1, 3, 4),
      from = c(1, 3, 1, 2)
    ))
  expect_length(get_type(gr, edge_is_loop()), igraph::gsize(gr))
  expect_length(get_type(gr, edge_is_multiple()), igraph::gsize(gr))
  expect_length(get_type(gr, edge_is_mutual()), igraph::gsize(gr))
})
test_that("edge types require edge active", {
  gr <- create_notable('bull') %>%
    activate(nodes) %>%
    bind_edges(tibble::tibble(
      to = c(1, 1, 3, 4),
      from = c(1, 3, 1, 2)
    ))
  expect_error(get_type(gr, edge_is_loop()))
  expect_error(get_type(gr, edge_is_multiple()))
  expect_error(get_type(gr, edge_is_mutual()))
})
