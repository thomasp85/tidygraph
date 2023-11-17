test_that("to_linegraph works", {
  gr <- create_notable('bull') %>%
    morph(to_linegraph) %>%
    activate(nodes) %>%
    mutate(id = seq_len(n()))
  gr1 <- unmorph(gr) %>% activate(edges)
  gr2 <- crystallise(gr)
  expect_equal(pull(gr1, id), 1:5)
  expect_equal(nrow(gr2), 1)
})
test_that('to_subgraph works', {
  gr <- create_notable('bull') %>%
    morph(to_subgraph, seq_len(n()) < 4) %>%
    mutate(selected = TRUE)
  gr1 <- unmorph(gr) %>% activate(nodes)
  gr2 <- crystallise(gr)
  expect_equal(pull(gr1, selected), c(TRUE, TRUE, TRUE, NA, NA))
  expect_equal(nrow(gr2), 1)
})
test_that('to_split works', {
  gr <- create_notable('bull') %>%
    mutate(group = c(1,1,1,2,2)) %>%
    morph(to_split, group) %>%
    mutate(size = graph_order())
  gr1 <- unmorph(gr) %>% activate(nodes)
  gr2 <- crystallise(gr)
  expect_equal(pull(gr1, size), c(3, 3, 3, 2, 2))
  expect_equal(nrow(gr2), 2)
})
test_that('to_components works', {
  gr <- create_notable('bull') %>%
    bind_graphs(create_notable('diamond')) %>%
    morph(to_components) %>%
    mutate(size = graph_order())
  gr1 <- unmorph(gr) %>% activate(nodes)
  gr2 <- crystallise(gr)
  expect_equal(pull(gr1, size), c(5, 5, 5, 5, 5, 4, 4, 4, 4))
  expect_equal(nrow(gr2), 2)
})
test_that('to_largest_component works', {
  gr <- create_notable('bull') %>%
    bind_graphs(create_notable('diamond')) %>%
    morph(to_largest_component) %>%
    mutate(size = graph_order())
  gr1 <- unmorph(gr) %>% activate(nodes)
  gr2 <- crystallise(gr)
  expect_equal(pull(gr1, size), c(5, 5, 5, 5, 5, NA, NA, NA, NA))
  expect_equal(nrow(gr2), 1)
})
test_that('to_local_neighborhood works', {
  gr <- create_notable('bull') %>%
    morph(to_local_neighborhood, 5) %>%
    mutate(selected = TRUE)
  gr1 <- unmorph(gr) %>% activate(nodes)
  gr2 <- crystallise(gr)
  expect_equal(pull(gr1, selected), c(NA, NA, TRUE, NA, TRUE))
  expect_equal(nrow(gr2), 1)
})
test_that('to_dominator_tree works', {
  gr <- create_ring(5, directed = TRUE) %>%
    morph(to_dominator_tree, 3) %>%
    mutate(order = node_topo_order())
  gr1 <- unmorph(gr) %>% activate(nodes)
  gr2 <- crystallise(gr)
  expect_equal(pull(gr1, order), c(4, 5, 1, 2, 3))
  expect_equal(nrow(gr2), 1)
})
test_that('to_minimum_spanning_tree works', {
  gr <- create_notable('bull') %>%
    morph(to_minimum_spanning_tree) %>%
    mutate(order = bfs_rank(3))
  gr1 <- unmorph(gr) %>% activate(nodes)
  gr2 <- crystallise(gr)
  expect_equal(pull(gr1, order), c(2, 4, 1, 5, 3))
  expect_equal(nrow(gr2), 1)
})
test_that('to_random_spanning_tree works', {
  set.seed(1)
  gr <- create_notable('bull') %>%
    morph(to_random_spanning_tree) %>%
    mutate(order = bfs_rank(3))
  gr1 <- unmorph(gr) %>% activate(nodes)
  gr2 <- crystallise(gr)
  expect_equal(pull(gr1, order), c(4, 2, 1, 5, 3))
  expect_equal(nrow(gr2), 1)
})
test_that('to_shortest_path works', {
  gr <- create_notable('bull') %>%
    morph(to_shortest_path, 4, 5) %>%
    mutate(selected = TRUE)
  gr1 <- unmorph(gr) %>% activate(nodes)
  gr2 <- crystallise(gr)
  expect_equal(pull(gr1, selected), c(NA, TRUE, TRUE, TRUE, TRUE))
  expect_equal(nrow(gr2), 1)
})
test_that('to_bfs_tree works', {
  gr <- create_notable('bull') %>%
    morph(to_bfs_tree, 5) %>%
    mutate(degree = centrality_degree())
  gr1 <- unmorph(gr) %>% activate(nodes)
  gr2 <- crystallise(gr)
  expect_equal(pull(gr1, degree), c(0, 1, 2, 0, 1))
  expect_equal(nrow(gr2), 1)
})
test_that('to_dfs_tree works', {
  gr <- create_notable('bull') %>%
    morph(to_dfs_tree, 5) %>%
    mutate(degree = centrality_degree())
  gr1 <- unmorph(gr) %>% activate(nodes)
  gr2 <- crystallise(gr)
  expect_equal(pull(gr1, degree), c(1, 1, 1, 0, 1))
  expect_equal(nrow(gr2), 1)
})
test_that('to_simple works', {
  gr <- create_ring(5, directed = TRUE)
  gr <- bind_edges(gr, as_tibble(gr, 'edges')) %>%
    morph(to_simple) %>%
    mutate(size = graph_size())
  gr1 <- unmorph(gr) %>% activate(nodes)
  gr2 <- crystallise(gr)
  expect_equal(pull(gr1, size), rep(5, 5))
  expect_equal(nrow(gr2), 1)
})
test_that('to_contracted works', {
  gr <- create_notable('bull') %>%
    mutate(group = c(1,1,1,2,2)) %>%
    morph(to_contracted, group) %>%
    mutate(node = rev(seq_len(n())))
  gr1 <- unmorph(gr) %>% activate(nodes)
  gr2 <- crystallise(gr)
  expect_equal(pull(gr1, node), c(2,2,2,1,1))
  expect_equal(nrow(gr2), 1)
})

test_empty_context()
