test_that("focusing behaves", {
  gr <- create_notable('meredith')
  expect_s3_class(focus(gr, ))
})
