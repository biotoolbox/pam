library(testthat)
library(pam)

test_that("test vollenweider", {
  test_data_file <- file.path(getwd(), "data", "20231122_10_W3_T20_ML.csv")
  message("using test data file: ", test_data_file)

  data <- read_pam_data(test_data_file)
  data_vollenweider <- generate_regression_vollenweider_ETR_I(data)
  View(data_vollenweider)

  expect_equal(1, 1)
})
