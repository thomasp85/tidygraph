context("distinct")

test_that("distinct works", {
  gr <- create_notable('bull') %>%
    mutate(id = c(1,1,1,2,2)) %>%
    activate(edges) %>%
    mutate(id = c(1,1,2,2,3))
  gr1 <- gr %>% activate(nodes) %>% distinct()
  expect_equal(igraph::gorder(gr1), 2)
  gr2 <- gr %>% activate(edges) %>% distinct(id)
  expect_equal(igraph::gsize(gr2), 3)
})
