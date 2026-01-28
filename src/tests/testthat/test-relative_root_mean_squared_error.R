test_that("test-relative_root_mean_squared_error.R", {
  test_data_file <- file.path(getwd(), "data", "20240925.csv")
  expect_no_error({
    data <- read_dual_pam_data(test_data_file)
    model_result <- walsby_generate_regression_ETR_II(data)
    etr_regression_data <- get_etr_regression_data_from_model_result(model_result)
    result <- get_etr_data_for_par_values(data, etr_regression_data, etr_2_type)

    relative_root_mean_squared_error <- relative_root_mean_squared_error(result)
  })

  if (is_supported_os() == FALSE) {
    skip("Unsupported operating system for this test.")
  }

  if (is_debian_or_ubuntu()) {
    expect_equal(relative_root_mean_squared_error, 0.0331529227874065)
  } else if (is_windows()) {
    expect_equal(relative_root_mean_squared_error, 0.0331529227874065)
  }
})
