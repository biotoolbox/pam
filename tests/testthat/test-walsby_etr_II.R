test_that("test walsby ETR II", {
  test_data_file <- file.path(getwd(), "data", "20231122_01_W3_T20_HL.csv")
  data <- read_pam_data(test_data_file)
  model_result <- generate_regression_walsby_ETR_II(data)

  expect_equal(model_result[["sdiff"]], 4.4596755)
  expect_equal(model_result[["etr_max"]], 47.999349)
  expect_equal(model_result[["alpha"]], 0.11605834)
  expect_equal(model_result[["beta"]], -0.012745, tolerance = 0.000001)
})

test_that("test plot walsby ETR II 20231122_01_W3_T20_HL.csv", {
  test_data_file <- file.path(getwd(), "data", "20231122_01_W3_T20_HL.csv")
  data <- read_pam_data(test_data_file)
  model_result <- generate_regression_walsby_ETR_II(data)

  expect_no_warning(
    print(
      plot_control_walsby(
        data,
        model_result,
        etr_II_type,
        "walsby ETR II 20231122_01_W3_T20_HL.csv"
      )
    )
  )
})
