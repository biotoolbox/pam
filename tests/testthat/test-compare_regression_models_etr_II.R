test_that("compare_regression_models etr II", {
  test_data_file <- file.path(getwd(), "data", "bulk")

  expect_no_warning(
    model_points_etr_II <- compare_regression_models_ETR_II(test_data_file)
  )

  expect_equal(model_points_etr_II[["eilers_peeters"]], 246)
  expect_equal(model_points_etr_II[["platt"]], 130)
  expect_equal(model_points_etr_II[["vollenweider"]], 304)
  expect_equal(model_points_etr_II[["walsby"]], 40)
})
