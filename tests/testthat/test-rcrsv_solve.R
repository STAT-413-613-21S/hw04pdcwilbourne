test_that("function works", {
  x <- rcrsv_solve(c(2, 4, 3), 4)
  expect_equal(x, 2.5)
})

test_that("x is of length 3", {
  expect_error(rcrsv_solve(c(2,3,4,5), 1))
})

test_that("n is not a list", {
  expect_error(rcrsv_solve(c(2,3,4), n = list(1)))
})

test_that("x is numeric", {
  expect_error(rcrsv_solve(c(2,3,"foo"), n = 5))
})

test_that("n is numeric", {
  expect_error(rcrsv_solve(c(2,3,4), n = TRUE))
})

test_that("n is positive", {
  expect_error(rcrsv_solve(c(2,3,4), n = -1))
})

test_that("n must be an integer", {
  expect_error(rcrsv_solve(c(2,3,4), n = 1.5))
})
