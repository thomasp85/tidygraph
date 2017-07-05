context("filter")

test_that("filter works", {
  id_nodes <- create_notable('bull') %>%
    mutate(id = seq_len(n())) %>%
    filter(id < 4) %>%
    pull(id)
  expect_equal(id_nodes, 1:3)
  id_edges <- create_notable('bull') %>%
    activate(edges) %>%
    mutate(id = seq_len(n())) %>%
    filter(id < 4) %>%
    pull(id)
  expect_equal(id_edges, 1:3)
})
