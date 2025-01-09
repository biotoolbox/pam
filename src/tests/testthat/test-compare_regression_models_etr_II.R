test_that("compare_regression_models etr II", {
  test_data_dir <- file.path(getwd(), "data", "bulk")
  
  os_name <- Sys.info()[["sysname"]]

  if (os_name == "Linux") {
    expect_no_error(
      model_points_etr_II <- compare_regression_models_ETR_II(test_data_dir)
    )

    expect_equal(model_points_etr_II[["eilers_peeters"]], 40)
    expect_equal(model_points_etr_II[["platt"]], 22)
    expect_equal(model_points_etr_II[["vollenweider"]], 40)
    expect_equal(model_points_etr_II[["walsby"]], 6)
  } else if (os_name == "Windows") {
    expect_no_error(
      model_points_etr_II <- compare_regression_models_ETR_II(test_data_dir)
    )

    expect_equal(model_points_etr_II[["eilers_peeters"]], 39)
    expect_equal(model_points_etr_II[["platt"]], 22)
    expect_equal(model_points_etr_II[["vollenweider"]], 41)
    expect_equal(model_points_etr_II[["walsby"]], 6)
  } else {
    stop(paste("Unsupported operating system:", os_name))
  }
})
