test_that("attributes are applied correctly", {
  gr1 <- create_notable('bull')
  gr2 <- create_notable('diamond')
  igraph::graph_attr(gr1, 'igraph_attr') <- 'test'
  attr(gr1, 'standard_attr') <- 'test2'
  gr1 <- as_tbl_graph(gr1)
  gr2 <- gr2 %gr_attr% gr1
  expect_equal(attributes(gr1), attributes(gr2))
  expect_equal(graph_attr(gr1), graph_attr(gr2))
})

test_empty_context()
