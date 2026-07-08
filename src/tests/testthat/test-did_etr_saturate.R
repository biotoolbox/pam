test_that("did_etr_saturate is TRUE when the predicted peak is before the highest PAR", {
  regression_data <- create_regression_data(
    pars = c(100, 200, 300, 400),
    predictions = c(10, 30, 20, 15)
  )

  expect_true(did_etr_saturate(regression_data))
})

test_that("did_etr_saturate is FALSE when predictions increase monotonically to the highest PAR", {
  regression_data <- create_regression_data(
    pars = c(100, 200, 300, 400),
    predictions = c(5, 10, 15, 20)
  )

  expect_false(did_etr_saturate(regression_data))
})

test_that("did_etr_saturate is FALSE when the maximum prediction occurs exactly at the highest PAR", {
  regression_data <- create_regression_data(
    pars = c(100, 200, 300),
    predictions = c(10, 20, 30)
  )

  expect_false(did_etr_saturate(regression_data))
})

test_that("did_etr_saturate is TRUE for a plateau where the maximum is first reached before the highest PAR", {
  regression_data <- create_regression_data(
    pars = c(100, 200, 300),
    predictions = c(30, 30, 20)
  )

  expect_true(did_etr_saturate(regression_data))
})

test_that("did_etr_saturate errors on invalid regression data", {
  expect_error(did_etr_saturate(NULL))

  invalid_regression_data <- data.table::data.table(
    foo = c(1, 2, 3),
    bar = c(4, 5, 6)
  )
  expect_error(did_etr_saturate(invalid_regression_data))
})
