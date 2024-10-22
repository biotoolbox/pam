test_that("test-vollenweider_etr_II generate regression 20231122_01_W3_T20_HL.csv", {
  test_data_file <- file.path(getwd(), "data", "20231122_01_W3_T20_HL.csv")
  data <- read_pam_data(test_data_file)
  model_result <- vollenweider_generate_regression_ETR_II(data)

  expect_equal(model_result[["sdiff"]], 3.04234525)
  expect_equal(model_result[["pmax"]], 36.644038)
  expect_equal(model_result[["a"]], 0.00243709, tolerance = 0.00001)
  expect_equal(model_result[["alpha"]], -0.00001386, tolerance = 0.0001)
  expect_equal(model_result[["n"]], 1059.2046)
  expect_equal(model_result[["ik"]], 410.32484)
  expect_equal(model_result[["popt"]], 30.70697)
  expect_equal(model_result[["iik"]], 343.844)
})

test_that("test-vollenweider_etr_II control plot 20231122_01_W3_T20_HL.csv", {
  test_data_file <- file.path(getwd(), "data", "20231122_01_W3_T20_HL.csv")
  data <- read_pam_data(test_data_file)
  model_result <- vollenweider_generate_regression_ETR_II(data)

  expect_no_warning(
    print(
      plot_control(
        data,
        model_result,
        "vollenweider ETR II 20231122_01_W3_T20_HL.csv",
        color_vollenweider
      )
    )
  )
})
