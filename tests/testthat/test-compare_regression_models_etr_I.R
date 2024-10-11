test_that("compare_regression_models etr I", {
  test_data_dir <- file.path(getwd(), "data", "bulk")

  expect_no_warning(
    model_points_etr_I <- compare_regression_models_ETR_I(test_data_dir)
  )

  expect_equal(model_points_etr_I[["eilers_peeters"]], 133)
  expect_equal(model_points_etr_I[["platt"]], 109)
  expect_equal(model_points_etr_I[["vollenweider"]], 188)
  expect_equal(model_points_etr_I[["walsby"]], 176)
})
