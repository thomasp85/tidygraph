n_val <- sample(70)
e_val <- sample(140)

gr <- create_notable("meredith") %>%
  mutate(id = seq_len(70), val = n_val) %>%
  activate(edges) %>%
  mutate(id = seq_len(140), val = e_val) %>%
  activate(nodes)

test_that("slicing nodes works", {
  expect_equal(slice(gr, 1:4) %>% pull(val), n_val[1:4])
  expect_equal(slice_head(gr, n = 6) %>% pull(val), n_val[1:6])
  expect_equal(slice_tail(gr, n = 6) %>% pull(val), rev(rev(n_val)[1:6]))
  expect_equal(slice_min(gr, val, n = 6) %>% pull(id), sort(order(n_val)[1:6]))
  expect_equal(slice_max(gr, val, n = 6) %>% pull(id), sort(order(n_val, decreasing = TRUE)[1:6]))
})

test_that("slicing edges works", {
  gr <- gr %>% activate(edges)
  expect_equal(slice(gr, 1:4) %>% pull(val), e_val[1:4])
  expect_equal(slice_head(gr, n = 6) %>% pull(val), e_val[1:6])
  expect_equal(slice_tail(gr, n = 6) %>% pull(val), rev(rev(e_val)[1:6]))
  expect_equal(slice_min(gr, val, n = 6) %>% pull(id), sort(order(e_val)[1:6]))
  expect_equal(slice_max(gr, val, n = 6) %>% pull(id), sort(order(e_val, decreasing = TRUE)[1:6]))
})

test_empty_context()
