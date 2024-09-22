test_that("compare_regression_models etr I + II", {
  test_data_file <- file.path(getwd(), "data", "bulk")

  model_points_etr_I <- compare_regression_models_ETR_I(test_data_file)
  model_points_etr_II <- compare_regression_models_ETR_II(test_data_file)

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
  View(result)

  expect_equal(1, 2)
})
