test_that("test vollenweider ETR I", {
  test_data_file <- file.path(getwd(), "data", "20231122_01_W3_T20_HL.csv")
  data <- read_pam_data(test_data_file)
  model_result <- generate_regression_vollenweider_ETR_I(data)

  expect_equal(model_result[["sdiff"]], 1099.94915)
  expect_equal(model_result[["pmax"]], 1132.09578)
  expect_equal(model_result[["a"]], 0.00016833, tolerance = 0.00001)
  expect_equal(model_result[["alpha"]], -0.00001382, tolerance = 0.0001)
  expect_equal(model_result[["n"]], 4224.5959)
  expect_equal(model_result[["ik"]], 5940.6891)
  expect_equal(model_result[["popt"]], 126.51602)
  expect_equal(model_result[["iik"]], 663.89465)
})

test_that("test plot vollenweider ETR I 20231122_01_W3_T20_HL.csv", {
  test_data_file <- file.path(getwd(), "data", "20231122_01_W3_T20_HL.csv")
  data <- read_pam_data(test_data_file)
  model_result <- generate_regression_vollenweider_ETR_I(data)

  expect_no_warning(
    print(
      plot_control_vollenweider(
        data,
        model_result,
        etr_I_type,
        "vollenweider ETR I 20231122_01_W3_T20_HL.csv"
      )
    )
  )
})
