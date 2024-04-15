test_that("compute_yld works", {

  res <- compute_yld()

  expect_true(is.numeric(res))

  expect_false(is.na(res))

})
