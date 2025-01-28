test_that("compare_regression_models etr II", {
  test_data_dir <- file.path(getwd(), "data", "bulk")
  
  os_name <- Sys.info()[["sysname"]]

  if (os_name == "Linux") {
    os_release <- readLines("/etc/os-release")
    distro_line <- grep("^ID=", os_release, value = TRUE)
    distro <- sub("^ID=", "", distro_line)

    if (distro == "debian" || distro == "ubuntu") {
      expect_no_error({
        model_points_etr_II <- compare_regression_models_ETR_II(test_data_dir)
      })

      expect_equal(model_points_etr_II[["eilers_peeters"]], 41)
      expect_equal(model_points_etr_II[["platt"]], 24)
      expect_equal(model_points_etr_II[["vollenweider"]], 40)
      expect_equal(model_points_etr_II[["walsby"]], 9)
    } else {
      skip(paste("Skipping test on unsupported Linux distribution:", distro))
    }
  } else if (os_name == "Windows") {
    expect_no_error({
      model_points_etr_II <- compare_regression_models_ETR_II(test_data_dir)
    })

    expect_equal(model_points_etr_II[["eilers_peeters"]], 40)
    expect_equal(model_points_etr_II[["platt"]], 24)
    expect_equal(model_points_etr_II[["vollenweider"]], 41)
    expect_equal(model_points_etr_II[["walsby"]], 9)
  } else {
    skip(paste("Skipping test on unsupported operating system:", os_name))
  }
})
