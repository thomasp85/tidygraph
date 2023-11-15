get_val <- function(gr, fn) {
  gr %>% mutate(val = fn) %>% pull(val)
}
test_that("search returns correct type", {
  gr <- create_tree(10, 2)
  expect_type(get_val(gr, bfs_after()), 'integer')
  expect_type(get_val(gr, bfs_before()), 'integer')
  expect_type(get_val(gr, bfs_dist()), 'integer')
  expect_type(get_val(gr, bfs_parent()), 'integer')
  expect_type(get_val(gr, bfs_rank()), 'integer')

  expect_type(get_val(gr, dfs_dist()), 'integer')
  expect_type(get_val(gr, dfs_parent()), 'integer')
  expect_type(get_val(gr, dfs_rank()), 'integer')
  expect_type(get_val(gr, dfs_rank_out()), 'integer')
})
test_that("search returns correct length", {
  gr <- create_tree(10, 2)
  expect_length(get_val(gr, bfs_after()), igraph::gorder(gr))
  expect_length(get_val(gr, bfs_before()), igraph::gorder(gr))
  expect_length(get_val(gr, bfs_dist()), igraph::gorder(gr))
  expect_length(get_val(gr, bfs_parent()), igraph::gorder(gr))
  expect_length(get_val(gr, bfs_rank()), igraph::gorder(gr))

  expect_length(get_val(gr, dfs_dist()), igraph::gorder(gr))
  expect_length(get_val(gr, dfs_parent()), igraph::gorder(gr))
  expect_length(get_val(gr, dfs_rank()), igraph::gorder(gr))
  expect_length(get_val(gr, dfs_rank_out()), igraph::gorder(gr))
})
test_that("search returns correct length", {
  gr <- create_tree(10, 2) |>
    focus(dplyr::row_number() < 3)
  expect_length(get_val(gr, bfs_after()), 2)
  expect_length(get_val(gr, bfs_before()), 2)
  expect_length(get_val(gr, bfs_dist()), 2)
  expect_length(get_val(gr, bfs_parent()), 2)
  expect_length(get_val(gr, bfs_rank()), 2)

  expect_length(get_val(gr, dfs_dist()), 2)
  expect_length(get_val(gr, dfs_parent()), 2)
  expect_length(get_val(gr, dfs_rank()), 2)
  expect_length(get_val(gr, dfs_rank_out()), 2)
})
test_that("search requires active nodes", {
  gr <- create_tree(10, 2) %>% activate(edges)
  expect_error(get_val(gr, bfs_after()))
  expect_error(get_val(gr, bfs_before()))
  expect_error(get_val(gr, bfs_dist()))
  expect_error(get_val(gr, bfs_parent()))
  expect_error(get_val(gr, bfs_rank()))

  expect_error(get_val(gr, dfs_dist()))
  expect_error(get_val(gr, dfs_parent()))
  expect_error(get_val(gr, dfs_rank()))
  expect_error(get_val(gr, dfs_rank_out()))
})

test_empty_context()
