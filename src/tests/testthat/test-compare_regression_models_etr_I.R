test_that("compare_regression_models etr I", {
  test_data_dir <- file.path(getwd(), "data", "bulk")

  if (is_supported_os() == FALSE) {
    skip("Unsupported operating system for this test.")
  }

  if (is_debian_or_ubuntu()) {
    expect_no_error({
      model_points_etr_I <- compare_regression_models_ETR_I(test_data_dir)
    })
    expect_equal(model_points_etr_I[["eilers_peeters"]], 13)
    expect_equal(model_points_etr_I[["platt"]], 16)
    expect_equal(model_points_etr_I[["vollenweider"]], 30)
    expect_equal(model_points_etr_I[["walsby"]], 19)
  } else if (is_windows()) {
    expect_no_error({
      model_points_etr_I <- compare_regression_models_ETR_I(test_data_dir)
    })
    expect_equal(model_points_etr_I[["eilers_peeters"]], 13)
    expect_equal(model_points_etr_I[["platt"]], 16)
    expect_equal(model_points_etr_I[["vollenweider"]], 30)
    expect_equal(model_points_etr_I[["walsby"]], 19)
  }
})
