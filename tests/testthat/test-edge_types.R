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
  expect_type(get_type(gr, edge_is_loop()), 'logical')
  expect_type(get_type(gr, edge_is_multiple()), 'logical')
  expect_type(get_type(gr, edge_is_mutual()), 'logical')
  expect_type(get_type(gr, edge_is_bridge()), 'logical')
  expect_type(get_type(gr, edge_is_feedback_arc()), 'logical')
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
  expect_length(get_type(gr, edge_is_bridge()), igraph::gsize(gr))
  expect_length(get_type(gr, edge_is_feedback_arc()), igraph::gsize(gr))
})

test_that("edge types return correct length for focus", {
  gr <- create_notable('bull') %>%
    activate(edges) %>%
    bind_edges(tibble::tibble(
      to = c(1, 1, 3, 4),
      from = c(1, 3, 1, 2)
    )) |>
    focus(dplyr::row_number() < 3)
  expect_length(get_type(gr, edge_is_loop()), 2)
  expect_length(get_type(gr, edge_is_multiple()), 2)
  expect_length(get_type(gr, edge_is_mutual()), 2)
  expect_length(get_type(gr, edge_is_bridge()), 2)
  expect_length(get_type(gr, edge_is_feedback_arc()), 2)
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
  expect_error(get_type(gr, edge_is_bridge()))
  expect_error(get_type(gr, edge_is_feedback_arc()))
})

test_empty_context()
