test_that("compare_regression_models etr I", {
  test_data_dir <- file.path(getwd(), "data", "bulk")
  
  os_name <- Sys.info()[["sysname"]]

  if (os_name == "Linux") {
    expect_no_error({
      model_points_etr_I <- compare_regression_models_ETR_I(test_data_dir)
    })

    expect_equal(model_points_etr_I[["eilers_peeters"]], 8)
    expect_equal(model_points_etr_I[["platt"]], 4)
    expect_equal(model_points_etr_I[["vollenweider"]], 10)
    expect_equal(model_points_etr_I[["walsby"]], 14)
  } else if (os_name == "Windows") {
    expect_no_error({
      model_points_etr_I <- compare_regression_models_ETR_I(test_data_dir)
    })

    expect_equal(model_points_etr_I[["eilers_peeters"]], 8)
    expect_equal(model_points_etr_I[["platt"]], 4)
    expect_equal(model_points_etr_I[["vollenweider"]], 10)
    expect_equal(model_points_etr_I[["walsby"]], 14)
  } else {
    stop(paste("Unsupported operating system:", os_name))
  }
})

