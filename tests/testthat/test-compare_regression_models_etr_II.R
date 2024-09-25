test_that("compare_regression_models etr II", {
  test_data_file <- file.path(getwd(), "data", "bulk")

  model_points_etr_II <- compare_regression_models_ETR_II(test_data_file)
  #print(model_points_etr_II)

  expect_equal(1, 1)
})
