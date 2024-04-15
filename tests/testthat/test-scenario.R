test_that("scenario works", {

  res <- scenario()

  expect_true(is.data.frame(res))

  numeric_output <- res$new_adopters

  expect_true(is.numeric(numeric_output))

})
