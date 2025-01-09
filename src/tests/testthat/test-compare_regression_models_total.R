test_that("compare_regression_models etr I + II", {
  test_data_dir <- file.path(getwd(), "data", "bulk")

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

  os_name <- Sys.info()[["sysname"]]

  if (os_name == "Linux") {
    expect_equal(result[["eilers_peeters_total"]], 48)
    expect_equal(result[["platt_total"]], 26)
    expect_equal(result[["vollenweider_total"]], 50)
    expect_equal(result[["walsby_total"]], 20)
  } else if (os_name == "Windows") {
    expect_equal(result[["eilers_peeters_total"]], 47)
    expect_equal(result[["platt_total"]], 26)
    expect_equal(result[["vollenweider_total"]], 51)
    expect_equal(result[["walsby_total"]], 20)
  } else {
    stop(paste("Unsupported operating system:", os_name))
  }
})

