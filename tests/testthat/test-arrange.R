test_that("arrange works with nodes", {
  ord <- c(2, 4, 1, 3, 5)
  gr1 <- create_notable('bull')
  gr1 <- mutate(gr1, name = letters[1:5], order = ord)
  gr1 <- arrange(gr1, order)
  expect_equal(pull(gr1, name), letters[1:5][match(1:5, ord)])
})
test_that("arrange works with edges", {
  ord <- c(2, 4, 1, 3, 5)
  gr1 <- activate(create_notable('bull'), edges)
  gr1 <- mutate(gr1, name = letters[1:5], order = ord)
  gr1 <- arrange(gr1, order)
  expect_equal(pull(gr1, name), letters[1:5][match(1:5, ord)])
})
test_that('reserved words are protected', {
  ord <- c(2, 4, 1, 3, 5)
  gr1 <- create_notable('bull')
  gr1 <- mutate(gr1, .tbl_graph_index = letters[1:5], order = ord)
  expect_error(arrange(gr1, order))
})

test_empty_context()
