test_that("function works", {
  x <- calcn(c(2, 4, 3), 4)
  expect_equal(x, 2.5)
})

test_that("x can only be of length 3", {
  expect_error(calcn(c(2,3,4,5), 1))
})

test_that("n must be an integer", {
  expect_error(calcn(c(2,3,4), n = 1.5))
})

