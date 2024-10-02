test_that("test walsby ETR I", {
  test_data_file <- file.path(getwd(), "data", "20231122_01_W3_T20_HL.csv")
  data <- read_pam_data(test_data_file)
  model_result <- generate_regression_walsby_ETR_I(data)

  expect_equal(model_result[["sdiff"]], 1247.65184)
  expect_equal(model_result[["etr_max"]], 3970930.3)
  expect_equal(model_result[["alpha"]], 27.690236)
  expect_equal(model_result[["beta"]], -27.4705392)
})

test_that("test plot walsby ETR I 20231122_01_W3_T20_HL.csv", {
  test_data_file <- file.path(getwd(), "data", "20231122_01_W3_T20_HL.csv")
  data <- read_pam_data(test_data_file)
  model_result <- generate_regression_walsby_ETR_I(data)

  expect_no_warning(
    print(
      plot_control_walsby(
        data,
        model_result,
        etr_I_type,
        "walsby ETR I 20231122_01_W3_T20_HL.csv"
      )
    )
  )
})
