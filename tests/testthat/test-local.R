get_loc <- function(gr, fn) {
  gr %>% mutate(loc = fn) %>% pull(loc)
}
test_that("local returns correct type", {
  gr <- create_notable('bull')
  expect_type(get_loc(gr, local_ave_degree()), 'double')
  expect_type(get_loc(gr, local_members()), 'list')
  expect_type(get_loc(gr, local_size()), 'double')
  expect_type(get_loc(gr, local_transitivity()), 'double')
  expect_type(get_loc(gr, local_triangles()), 'double')
})
test_that("local returns correct length", {
  gr <- create_notable('bull')
  expect_length(get_loc(gr, local_ave_degree()), igraph::gorder(gr))
  expect_length(get_loc(gr, local_members()), igraph::gorder(gr))
  expect_length(get_loc(gr, local_size()), igraph::gorder(gr))
  expect_length(get_loc(gr, local_transitivity()), igraph::gorder(gr))
  expect_length(get_loc(gr, local_triangles()), igraph::gorder(gr))
})
test_that("local returns correct length for focus", {
  gr <- create_notable('bull') |> focus(dplyr::row_number() < 3)
  expect_length(get_loc(gr, local_ave_degree()), 2)
  expect_length(get_loc(gr, local_members()), 2)
  expect_length(get_loc(gr, local_size()), 2)
  expect_length(get_loc(gr, local_transitivity()), 2)
  expect_length(get_loc(gr, local_triangles()), 2)
})
test_that("local requires active nodes", {
  gr <- create_notable('bull') %>% activate(edges)
  expect_error(get_loc(gr, local_ave_degree()))
  expect_error(get_loc(gr, local_members()))
  expect_error(get_loc(gr, local_size()))
  expect_error(get_loc(gr, local_transitivity()))
  expect_error(get_loc(gr, local_triangles()))
})

test_empty_context()
