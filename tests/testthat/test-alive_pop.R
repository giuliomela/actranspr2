test_that("alive_pop works", {

  starting_pop <- 50

  death_rate <- c(rep(0.01, 10))

  res <- alive_pop(starting_pop = starting_pop,
                   death_rate = death_rate)

  expect_true(is.numeric(res))

  expect_true(res[length(res)] < res[1])

})
