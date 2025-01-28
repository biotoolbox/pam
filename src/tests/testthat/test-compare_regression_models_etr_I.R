test_that("compare_regression_models etr I", {
  test_data_dir <- file.path(getwd(), "data", "bulk")
  
  os_name <- Sys.info()[["sysname"]]

  if (os_name == "Linux") {
    os_release <- readLines("/etc/os-release")
    distro_line <- grep("^ID=", os_release, value = TRUE)
    distro <- sub("^ID=", "", distro_line)

    if (distro == "debian" || distro == "ubuntu") {
      expect_no_error({
        model_points_etr_I <- compare_regression_models_ETR_I(test_data_dir)
      })

      expect_equal(model_points_etr_I[["eilers_peeters"]], 13)
      expect_equal(model_points_etr_I[["platt"]], 16)
      expect_equal(model_points_etr_I[["vollenweider"]], 30)
      expect_equal(model_points_etr_I[["walsby"]], 19)
    } else {
      skip(paste("Skipping test on unsupported Linux distribution:", distro))
    }
  } else if (os_name == "Windows") {
    expect_no_error({
      model_points_etr_I <- compare_regression_models_ETR_I(test_data_dir)
    })

    expect_equal(model_points_etr_I[["eilers_peeters"]], 13)
    expect_equal(model_points_etr_I[["platt"]], 16)
    expect_equal(model_points_etr_I[["vollenweider"]], 30)
    expect_equal(model_points_etr_I[["walsby"]], 19)
  } else {
    skip(paste("Skipping test on unsupported operating system:", os_name))
  }
})