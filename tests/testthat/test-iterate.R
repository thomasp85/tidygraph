test_that("iterate_n works as expected", {
  gr <- create_notable('zachary') |>
    activate(edges) |>
    mutate(count = 0) |>
    activate(nodes) |>
    iterate_n(10, ~mutate(activate(., edges), count = count + 1))

  expect_equal(active(gr), 'nodes')
  expect_true(all(pull(activate(gr, edges), count) == 10))
})

test_that("iterate_while works as expected", {
  gr <- create_notable('zachary') |>
    activate(edges) |>
    mutate(count = 0) |>
    activate(nodes) |>
    iterate_while(.E()$count[1] < 10, ~mutate(activate(., edges), count = count + 1))

  expect_equal(active(gr), 'nodes')
  expect_true(all(pull(activate(gr, edges), count) == 10))

  gr <- create_notable('zachary') |>
    activate(edges) |>
    mutate(count = 0) |>
    activate(nodes) |>
    iterate_while(TRUE, ~mutate(activate(., edges), count = count + 1), max_n = 10)

  expect_equal(active(gr), 'nodes')
  expect_true(all(pull(activate(gr, edges), count) == 10))
})
