context("bind")

test_that("bind_graphs works", {
  gr1 <- create_notable('bull')
  gr2 <- gr1
  gr1 <- mutate(gr1, group = 1)
  gr2 <- mutate(gr2, group = 2)
  gr <- bind_graphs(gr1, gr2)
  expect_equal(igraph::gorder(gr), 10)
  expect_equal(igraph::gsize(gr), 10)
  gr <- mutate(gr, comp = group_components())
  tbl <- as_tibble(gr)
  expect_true(all(lengths(lapply(split(tbl$group, tbl$comp), unique)) == 1))
})
test_that('bind_nodes works', {
  gr1 <- tbl_graph(head(mtcars))
  gr1 <- bind_nodes(gr1, tail(mtcars))
  tbl <- dplyr::bind_rows(head(mtcars), tail(mtcars))
  expect_equal(as_tibble(gr1), tbl)
})
test_that('bind_edges works', {
  gr1 <- create_notable('bull') %>%
    activate(edges) %>%
    mutate(id = 1:5, filter = c(TRUE, TRUE, TRUE, FALSE, FALSE))
  tbl <- as_tibble(gr1)
  gr2 <- filter(gr1, filter) %>%
    bind_edges(tbl[!tbl$filter, ])
  expect_equal(as_tibble(gr2), tbl)
  expect_error(bind_edges(gr2, tbl[, -(1:2)]))
  tbl2 <- tbl
  tbl2$to <- tbl2$to + 10
  expect_error(bind_edges(gr2, tbl2))
})
