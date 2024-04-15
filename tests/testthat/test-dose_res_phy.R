test_that("dose_res_phy works", {

  res <- dose_res_phy()

  expect_true(is.data.frame(res))

  expect_true(is.numeric(res[[2]]))


})
