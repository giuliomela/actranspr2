test_that("inhaled_dose works", {

  res <- inhaled_dose()

  expect_true(is.numeric(res))

  expect_false(is.na(res))

  expect_error(
    inhaled_dose(experimental_data = T, mode = "car")
  )

})
