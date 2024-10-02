test_that("test platt ETR II 20231122_01_W3_T20_HL.csv", {
  test_data_file <- file.path(getwd(), "data", "20231122_01_W3_T20_HL.csv")
  data <- read_pam_data(test_data_file)
  model_result <- generate_regression_platt_ETR_II(data)
  print(model_result)

  expect_equal(model_result[["sdiff"]], 4.4844798)
  expect_equal(model_result[["alpha"]], 0.1028022)
  expect_equal(model_result[["beta"]], 0.02631246, tolerance = 0.000001)
  expect_equal(model_result[["ps"]], 58.592426)
  expect_equal(model_result[["pm"]], 31.049374)
  expect_equal(model_result[["ik"]], 302.03024)
  expect_equal(model_result[["is"]], 569.95303)
  expect_equal(model_result[["ib"]], 2226.79356)
  expect_equal(model_result[["im"]], 906.60042)
})

test_that("test plot platt ETR II 20231122_01_W3_T20_HL.csv", {
  test_data_file <- file.path(getwd(), "data", "20231122_01_W3_T20_HL.csv")
  data <- read_pam_data(test_data_file)
  model_result <- generate_regression_platt_ETR_II(data)

  expect_no_warning(
    print(
      plot_control_platt(
        data,
        model_result,
        etr_II_type,
        "platt ETR II 20231122_01_W3_T20_HL.csv"
      )
    )
  )
})
