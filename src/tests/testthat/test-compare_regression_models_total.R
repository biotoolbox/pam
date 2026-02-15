test_that("compare_regression_models etr I + II - linux", {
  skip_if_not(is_debian_or_ubuntu())
  test_data_dir <- testthat::test_path("data", "bulk")
  model_points_etr_I <- compare_regression_models_ETR_I(test_data_dir, read_dual_pam_data)
  model_points_etr_II <- compare_regression_models_ETR_II(test_data_dir, read_dual_pam_data)

  eilers_peeters_total <- model_points_etr_I[["eilers_peeters"]] + model_points_etr_II[["eilers_peeters"]]
  platt_total <- model_points_etr_I[["platt"]] + model_points_etr_II[["platt"]]
  vollenweider_total <- model_points_etr_I[["vollenweider"]] + model_points_etr_II[["vollenweider"]]
  walsby_total <- model_points_etr_I[["walsby"]] + model_points_etr_II[["walsby"]]

  expect_equal(eilers_peeters_total, 50)
  expect_equal(platt_total, 39)
  expect_equal(vollenweider_total, 72)
  expect_equal(walsby_total, 31)
})

test_that("compare_regression_models etr I + II", {
  test_data_dir <- testthat::test_path("data", "bulk")

  model_points_etr_I <- compare_regression_models_ETR_I(test_data_dir, read_dual_pam_data)
  expect_named(model_points_etr_I, c("eilers_peeters", "platt", "vollenweider", "walsby"), ignore.order = TRUE)

  model_points_etr_II <- compare_regression_models_ETR_II(test_data_dir, read_dual_pam_data)
  expect_named(model_points_etr_II, c("eilers_peeters", "platt", "vollenweider", "walsby"), ignore.order = TRUE)

  eilers_peeters_total <- model_points_etr_I[["eilers_peeters"]] + model_points_etr_II[["eilers_peeters"]]
  expect_true(eilers_peeters_total >= 0)

  platt_total <- model_points_etr_I[["platt"]] + model_points_etr_II[["platt"]]
  expect_true(platt_total >= 0)

  vollenweider_total <- model_points_etr_I[["vollenweider"]] + model_points_etr_II[["vollenweider"]]
  expect_true(vollenweider_total >= 0)

  walsby_total <- model_points_etr_I[["walsby"]] + model_points_etr_II[["walsby"]]
  expect_true(walsby_total >= 0)
})
