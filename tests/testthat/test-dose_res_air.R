test_that("dose_res_air works", {

res <- dose_res_air()

res_exp <- dose_res_air(
  experimental_data = T
)

expect_true(is.data.frame(res))

expect_true(is.data.frame(res_exp))

expect_true(is.numeric(res[[2]]))

expect_true(is.numeric(res_exp[[2]]))


})
