test_that("compare_regression_models etr I", {
  test_data_file <- file.path(getwd(), "data", "bulk")

  model_points_etr_I <- compare_regression_models_ETR_I(test_data_file)
  View(model_points_etr_I, "I")

  expect_equal(1, 2)
})
