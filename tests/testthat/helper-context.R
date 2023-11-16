test_empty_context <- function() {
  test_that("graph context is empty after test", {
    expect_length(environment(.graph_context$free)$private$context, 0)
  })
}
