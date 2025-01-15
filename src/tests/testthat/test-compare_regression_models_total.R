test_that("compare_regression_models etr I + II", {
  test_data_dir <- file.path(getwd(), "data", "bulk")
  os_name <- Sys.info()[["sysname"]]

  if (os_name == "Linux") {
    os_release <- readLines("/etc/os-release")
    distro_line <- grep("^ID=", os_release, value = TRUE)
    distro <- sub("^ID=", "", distro_line)

    if (distro == "debian" || distro == "ubuntu") {
      expect_no_error({
        model_points_etr_I <- compare_regression_models_ETR_I(test_data_dir)
        model_points_etr_II <- compare_regression_models_ETR_II(test_data_dir)

        eilers_peeters_total <- model_points_etr_I[["eilers_peeters"]] + model_points_etr_II[["eilers_peeters"]]
        platt_total <- model_points_etr_I[["platt"]] + model_points_etr_II[["platt"]]
        vollenweider_total <- model_points_etr_I[["vollenweider"]] + model_points_etr_II[["vollenweider"]]
        walsby_total <- model_points_etr_I[["walsby"]] + model_points_etr_II[["walsby"]]

        result <- c(
          eilers_peeters_total = eilers_peeters_total,
          platt_total = platt_total,
          vollenweider_total = vollenweider_total,
          walsby_total = walsby_total
        )
      })

      expect_equal(result[["eilers_peeters_total"]], 48)
      expect_equal(result[["platt_total"]], 26)
      expect_equal(result[["vollenweider_total"]], 50)
      expect_equal(result[["walsby_total"]], 20)
    } else {
      skip(paste("Skipping test on unsupported Linux distribution:", distro))
    }
  } else if (os_name == "Windows") {
    expect_no_error({
      model_points_etr_I <- compare_regression_models_ETR_I(test_data_dir)
      model_points_etr_II <- compare_regression_models_ETR_II(test_data_dir)

      eilers_peeters_total <- model_points_etr_I[["eilers_peeters"]] + model_points_etr_II[["eilers_peeters"]]
      platt_total <- model_points_etr_I[["platt"]] + model_points_etr_II[["platt"]]
      vollenweider_total <- model_points_etr_I[["vollenweider"]] + model_points_etr_II[["vollenweider"]]
      walsby_total <- model_points_etr_I[["walsby"]] + model_points_etr_II[["walsby"]]

      result <- c(
        eilers_peeters_total = eilers_peeters_total,
        platt_total = platt_total,
        vollenweider_total = vollenweider_total,
        walsby_total = walsby_total
      )
    })

    expect_equal(result[["eilers_peeters_total"]], 47)
    expect_equal(result[["platt_total"]], 26)
    expect_equal(result[["vollenweider_total"]], 51)
    expect_equal(result[["walsby_total"]], 20)
  } else {
    skip(paste("Skipping test on unsupported operating system:", os_name))
  }
})
