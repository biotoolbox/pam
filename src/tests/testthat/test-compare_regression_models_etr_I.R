test_that("compare_regression_models etr I - linux", {
  skip_if_not(is_debian_or_ubuntu())
  test_data_dir <- file.path(getwd(), "data", "bulk")
  model_points_etr_1 <- compare_regression_models_ETR_I(test_data_dir, read_dual_pam_data)

  expect_equal(model_points_etr_I[["eilers_peeters"]], 13)
  expect_equal(model_points_etr_I[["platt"]], 19)
  expect_equal(model_points_etr_I[["vollenweider"]], 33)
  expect_equal(model_points_etr_I[["walsby"]], 25)
})

test_that("compare_regression_models etr I", {
  test_data_dir <- file.path(getwd(), "data", "bulk")
  model_points_etr_1 <- compare_regression_models_ETR_I(test_data_dir, read_dual_pam_data)
  expect_named(result, c("eilers_peeters", "platt", "vollenweider", "walsby"), ignore.order = TRUE)
})
