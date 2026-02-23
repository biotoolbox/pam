test_that("test-root_mean_squared_error - linux", {
  skip_if_not(is_debian_or_ubuntu())

  test_data_file <- testthat::test_path("data", "20240925.csv")
  data <- read_dual_pam_data(test_data_file)
  model_result <- walsby_generate_regression_ETR_II(data)
  etr_regression_data <- get_etr_regression_data_from_model_result(model_result)
  result <- get_etr_data_for_par_values(data, etr_regression_data, etr_2_type)

  root_mean_squared_error <- root_mean_squared_error(result)
  expect_equal(root_mean_squared_error, 0.88523222)
})

test_that("test-root_mean_squared_error", {
  test_data_file <- testthat::test_path("data", "20240925.csv")
  data <- read_dual_pam_data(test_data_file)
  model_result <- walsby_generate_regression_ETR_II(data)
  etr_regression_data <- get_etr_regression_data_from_model_result(model_result)
  result <- get_etr_data_for_par_values(data, etr_regression_data, etr_2_type)

  root_mean_squared_error <- root_mean_squared_error(result)
  expect_true(is.finite(root_mean_squared_error))
})
