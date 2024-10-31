test_that("test-write_model_result_csv 20231122_01_W3_T20_HL.csv", {
  test_data_file <- file.path(getwd(), "data", "20231122_01_W3_T20_HL.csv")
  data <- read_dual_pam_data(test_data_file)
  model_result <- walsby_generate_regression_ETR_II(data)
  model_result <- walsby_modified(model_result)

  result_dir <- file.path(getwd(), "results")
  dir.create(result_dir)

  expect_no_warning(
    write_model_result_csv(
      result_dir,
      "20231122_01_W3_T20_HL_ETR_II",
      data,
      model_result
    )
  )
})
