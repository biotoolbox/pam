test_that("compare_regression_models etr I + II", {
  test_data_dir <- file.path(getwd(), "data", "bulk")

  if (is_supported_os() == FALSE) {
    skip("Unsupported operating system for this test.")
  }


  if (is_debian_or_ubuntu()) {
    expect_no_error({
      model_points_etr_I <- compare_regression_models_ETR_I(test_data_dir, read_dual_pam_data)
      model_points_etr_II <- compare_regression_models_ETR_II(test_data_dir, read_dual_pam_data)

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

    expect_equal(result[["eilers_peeters_total"]], 50)
    expect_equal(result[["platt_total"]], 39)
    expect_equal(result[["vollenweider_total"]], 72)
    expect_equal(result[["walsby_total"]], 31)
  } else if (is_windows()) {
    expect_no_error({
      model_points_etr_I <- compare_regression_models_ETR_I(test_data_dir, read_dual_pam_data)
      model_points_etr_II <- compare_regression_models_ETR_II(test_data_dir, read_dual_pam_data)

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

    expect_equal(result[["eilers_peeters_total"]], 50)
    expect_equal(result[["platt_total"]], 39)
    expect_equal(result[["vollenweider_total"]], 72)
    expect_equal(result[["walsby_total"]], 31)
  }
})
