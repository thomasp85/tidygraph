test_that("random_walk_rank returns correct data", {
  set.seed(1)
  node_walk <- create_notable('zachary') |>
    mutate(walk_rank = random_walk_rank(30, 5)) |>
    pull(walk_rank)

  edge_walk <- create_notable('zachary') |>
    activate(edges) |>
    mutate(walk_rank = random_walk_rank(30, 5)) |>
    pull(walk_rank)

  expect_length(node_walk, 34)
  expect_length(edge_walk, 78)
  expect_type(node_walk, 'list')
  expect_type(edge_walk, 'list')
  expect_equal(node_walk[[5]], c(1, 12))
  expect_equal(node_walk[[2]], integer())
  expect_equal(edge_walk[[1]], integer())
  skip_on_cran()
  expect_equal(edge_walk[[36]], 1:4)
})
