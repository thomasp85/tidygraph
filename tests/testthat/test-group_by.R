context("group_by")

test_that("nodes and edges are grouped", {
  gr <- create_notable('bull') %>%
    mutate(group = c(1,1,1,2,2)) %>%
    group_by(group)
  expect_s3_class(as_tibble(gr), 'grouped_df')
  expect_false(dplyr::is_grouped_df(as_tibble(gr, 'edges')))
  gr <- gr %>%
    activate(edges) %>%
    mutate(group = c(1,1,1,2,2)) %>%
    group_by(group)
  expect_s3_class(as_tibble(gr), 'grouped_df')
  expect_false(dplyr::is_grouped_df(as_tibble(gr, 'nodes')))
})
