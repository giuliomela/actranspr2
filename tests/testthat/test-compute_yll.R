test_that("compute_yll works", {

  res <- compute_yll()

  expect_true(is.numeric(res))

  expect_equal(res, 14.86029, tolerance = 10e-4)

})
