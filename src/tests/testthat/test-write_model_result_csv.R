test_that("test-write_model_result_csv 20240925.csv", {
  test_data_file <- file.path(getwd(), "data", "20240925.csv")
  data <- read_dual_pam_data(test_data_file)
  model_result <- walsby_generate_regression_ETR_II(data)
  model_result <- walsby_modified(model_result)

  result_dir <- file.path(getwd(), "results")
  dir.create(result_dir)

  expect_no_warning(
    write_model_result_csv(
      result_dir,
      "20240925",
      data,
      model_result
    )
  )
})
