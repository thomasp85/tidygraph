test_that("active<- and activate works for tbl_graph", {
  gr1 <- create_notable('bull')
  gr1 <- activate(gr1, edges)
  expect_equal(active(gr1), 'edges')
  gr1 <- activate(gr1, 'nodes')
  expect_equal(active(gr1), 'nodes')
  test <- 'nodes'
  expect_error(activate(gr1, test))
  expect_equal(active(activate(gr1, !!test)), 'nodes')
  active(gr1) <- 'links'
  expect_equal(active(gr1), 'edges')
  active(gr1) <- 'vertices'
  expect_equal(active(gr1), 'nodes')
  expect_error(active(gr1) <- 'test')
})
test_that('activate ungroups', {
  gr1 <- mutate(create_notable('bull'), group = sample(1:2, n(), TRUE))
  gr1 <- group_by(gr1, group)
  expect_message(activate(gr1, edges))
  expect_equal(class(activate(gr1, edges)), c('tbl_graph', 'igraph'))
})
test_that('activate activates all morphed graphs', {
  gr1 <- gr1 <- mutate(create_notable('bull'), group = sample(1:2, n(), TRUE))
  gr1 <- morph(gr1, to_split, group)
  gr1 <- activate(gr1, 'edges')
  expect_true(all(sapply(gr1, active) == 'edges'))
})

test_empty_context()
