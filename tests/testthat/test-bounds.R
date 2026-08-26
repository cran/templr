library(testthat)

test_that("from01 scales [0,1] to [min,max]", {
  X <- data.frame(x = c(0, 0.5, 1))
  inp <- list(x = list(min = 10, max = 20))
  expect_equal(from01(X, inp)$x, c(10, 15, 20))
})

test_that("to01 scales [min,max] to [0,1]", {
  X <- data.frame(x = c(10, 15, 20))
  inp <- list(x = list(min = 10, max = 20))
  expect_equal(to01(X, inp)$x, c(0, 0.5, 1))
})

test_that("from01/to01 are inverse of each other", {
  X <- data.frame(x = runif(10))
  inp <- list(x = list(min = 10, max = 20))
  expect_equal(to01(from01(X, inp), inp), X)
})

test_that("from01 errors on missing bound info", {
  X <- data.frame(x = c(0, 1))
  expect_error(from01(X, list()), "min.*max")
})

test_that("to01 errors on missing bound info", {
  X <- data.frame(x = c(0, 1))
  expect_error(to01(X, list(x = list(min = 0))), "min.*max")
})

test_that("min_input/max_input extract bounds", {
  inp <- list(x1 = list(min = 0, max = 1), x2 = list(min = 2, max = 3))
  expect_equal(unname(min_input(inp)), c(0, 2))
  expect_equal(unname(max_input(inp)), c(1, 3))
})

test_that("min_input/max_input error on missing bound", {
  expect_error(min_input(list(x1 = list(max = 1))), "min")
  expect_error(max_input(list(x1 = list(min = 0))), "max")
})
