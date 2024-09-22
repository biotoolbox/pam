test_that("compare_regression_models etr II", {
  test_data_file <- file.path(getwd(), "data", "bulk")

  model_points_etr_II <- compare_regression_models_ETR_II(test_data_file)
  View(model_points_etr_II, "II")

  expect_equal(1, 2)
})
