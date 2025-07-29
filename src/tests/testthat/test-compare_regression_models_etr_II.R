test_that("compare_regression_models etr II", {
  test_data_dir <- file.path(getwd(), "data", "bulk")

  if (is_supported_os() == FALSE) {
    skip("Unsupported operating system for this test.")
  }

  if (is_debian_or_ubuntu()) {
    expect_no_error({
      model_points_etr_II <- compare_regression_models_ETR_II(test_data_dir)
    })
    expect_equal(model_points_etr_II[["eilers_peeters"]], 41)
    expect_equal(model_points_etr_II[["platt"]], 24)
    expect_equal(model_points_etr_II[["vollenweider"]], 40)
    expect_equal(model_points_etr_II[["walsby"]], 9)
  } else if (is_windows()) {
    expect_no_error({
      model_points_etr_II <- compare_regression_models_ETR_II(test_data_dir)
    })
    expect_equal(model_points_etr_II[["eilers_peeters"]], 40)
    expect_equal(model_points_etr_II[["platt"]], 24)
    expect_equal(model_points_etr_II[["vollenweider"]], 41)
    expect_equal(model_points_etr_II[["walsby"]], 9)
  }
})
