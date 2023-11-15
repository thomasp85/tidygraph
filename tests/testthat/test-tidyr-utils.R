gr <- create_notable('house') %>%
  mutate(val = c(1:3, NA, NA)) %>%
  activate(edges) %>%
  mutate(val = c(1:3, NA, NA, NA)) %>%
  activate(nodes)

test_that("tidyr utils work on nodes", {
  expect_equal(drop_na(gr) %>% pull(val), 1:3)
  expect_equal(replace_na(gr, list(val = 0)) %>% pull(val), c(1:3, 0, 0))
})

test_that("tidyr utils work on edges", {
  gr <- gr %>% activate(edges)
  expect_equal(drop_na(gr) %>% pull(val), 1:3)
  expect_equal(replace_na(gr, list(val = 0)) %>% pull(val), c(1:3, 0, 0, 0))
})

test_empty_context()
