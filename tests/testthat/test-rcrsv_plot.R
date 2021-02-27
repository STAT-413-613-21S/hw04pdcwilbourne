test_that("input is a list/dataframe", {
  my_data <- "foo"
  expect_error(rcrsv_plot(my_data))
})

test_that("input has four columns", {
  my_data <- tibble::tribble(
    ~x, ~y, ~z, ~n, ~p,
    2,4,3,3,5,
    2,4,3,4,5,
    2,4,3,5,5,
    2,4,3,6,5,
    2,4,3,7,5,
    2,4,3,8,5,
    2,4,3,9,5,
    2,4,3,10,5,
    2,4,3,12,5)
  expect_error(rcrsv_plot(my_data))
})

test_that("columns are numeric", {
  my_data <- tibble::tribble(
    ~x, ~y, ~z, ~n,
    2,4,3,"foo",
    2,4,3,"foo",
    2,4,3,"foo",
    2,4,3,"foo",
    2,4,3,"foo",
    2,4,3,"foo",
    2,4,3,"foo",
    2,4,3,"foo",
    2,4,3,"foo",)
  expect_error(rcrsv_plot(my_data))
})

test_that("column 4 is greater than zero", {
  my_data <- tibble::tribble(
    ~x, ~y, ~z, ~n,
    2,4,3,-1,
    2,4,3,4,
    2,4,3,5,
    2,4,3,6,
    2,4,3,7,
    2,4,3,8,
    2,4,3,9,
    2,4,3,10,
    2,4,3,12)
  expect_error(rcrsv_plot(my_data))
})

test_that("column four is an integer", {
  my_data <- tibble::tribble(
    ~x, ~y, ~z, ~n,
    2,4,3,3.5,
    2,4,3,4,
    2,4,3,5,
    2,4,3,6,
    2,4,3,7,
    2,4,3,8,
    2,4,3,9,
    2,4,3,10,
    2,4,3,12)
  expect_error(rcrsv_plot(my_data))
})

