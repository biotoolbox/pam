test_that("test platt ETR I 20231122_01_W3_T20_HL.csv", {
  test_data_file <- file.path(getwd(), "data", "20231122_01_W3_T20_HL.csv")
  data <- read_pam_data(test_data_file)
  model_result <- generate_regression_platt_ETR_I(data)

  expect_equal(model_result[["sdiff"]], 1709.3953)
  expect_equal(model_result[["alpha"]], 0.23751254, tolerance = 0.0000001)
  expect_equal(model_result[["beta"]], 38.788443)
  expect_equal(model_result[["ps"]], 56442.6305)
  expect_equal(model_result[["pm"]], 126.756436)
  expect_equal(model_result[["ik"]], 533.68312)
  expect_equal(model_result[["is"]], 237640.63)
  expect_equal(model_result[["ib"]], 1455.1404)
  expect_equal(model_result[["im"]], 1450.7034)
})

test_that("test plot platt ETR I 20231122_01_W3_T20_HL.csv", {
  test_data_file <- file.path(getwd(), "data", "20231122_01_W3_T20_HL.csv")
  data <- read_pam_data(test_data_file)
  model_result <- generate_regression_platt_ETR_I(data)

  expect_no_warning(
    print(
      plot_control_platt(
        data,
        model_result,
        etr_I_type,
        "platt ETR I 20231122_01_W3_T20_HL.csv"
      )
    )
  )
})
