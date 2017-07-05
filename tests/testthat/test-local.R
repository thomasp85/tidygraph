context("local")

get_loc <- function(gr, fn) {
  gr %>% mutate(loc = fn) %>% pull(loc)
}
test_that("local returns correct type", {
  gr <- create_notable('bull')
  expect_is(get_loc(gr, local_ave_degree()), 'numeric')
  expect_is(get_loc(gr, local_members()), 'list')
  expect_is(get_loc(gr, local_size()), 'numeric')
  expect_is(get_loc(gr, local_transitivity()), 'numeric')
  expect_is(get_loc(gr, local_triangles()), 'numeric')
})
test_that("local returns correct type", {
  gr <- create_notable('bull')
  expect_length(get_loc(gr, local_ave_degree()), igraph::gorder(gr))
  expect_length(get_loc(gr, local_members()), igraph::gorder(gr))
  expect_length(get_loc(gr, local_size()), igraph::gorder(gr))
  expect_length(get_loc(gr, local_transitivity()), igraph::gorder(gr))
  expect_length(get_loc(gr, local_triangles()), igraph::gorder(gr))
})
test_that("local requires active nodes", {
  gr <- create_notable('bull') %>% activate(edges)
  expect_error(get_loc(gr, local_ave_degree()))
  expect_error(get_loc(gr, local_members()))
  expect_error(get_loc(gr, local_size()))
  expect_error(get_loc(gr, local_transitivity()))
  expect_error(get_loc(gr, local_triangles()))
})
