context("mutate")

test_that("mutate works with nodes", {
  mut <- create_notable('bull') %>%
    mutate(letters = letters[1:5]) %>%
    pull(letters)
  expect_equal(mut, letters[1:5])
})
test_that("mutate works with edges", {
  mut <- create_notable('bull') %>%
    activate(edges) %>%
    mutate(letters = letters[1:5]) %>%
    pull(letters)
  expect_equal(mut, letters[1:5])
})
