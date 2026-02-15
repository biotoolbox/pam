test_that("test-root_mean_squared_error.R", {
  expect_no_error({
    test_data_file <- file.path(getwd(), "data", "20240925.csv")
    data <- read_dual_pam_data(test_data_file)
    model_result <- walsby_generate_regression_ETR_II(data)
    etr_regression_data <- get_etr_regression_data_from_model_result(model_result)
    result <- get_etr_data_for_par_values(data, etr_regression_data, etr_2_type)

    root_mean_squared_error <- root_mean_squared_error(result)
  })

  if (is_debian_or_ubuntu()) {
    expect_equal(root_mean_squared_error, 0.88523222)
  }
})
