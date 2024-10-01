test_that("test vollenweider ETR I", {
  test_data_file <- file.path(getwd(), "data", "20231122_01_W3_T20_HL.csv")
  data <- read_pam_data(test_data_file)
  reg_data <- generate_regression_vollenweider_ETR_I(data)
  
  expect_equal(reg_data[["sdiff"]], 1099.94915)
  expect_equal(reg_data[["pmax"]], 1132.09578)
  expect_equal(reg_data[["a"]], 0.00016833, tolerance = 0.00001)
  expect_equal(reg_data[["alpha"]], -0.00001382, tolerance = 0.0001)
  expect_equal(reg_data[["n"]], 4224.5959)
  expect_equal(reg_data[["ik"]], 5940.6891)
  expect_equal(reg_data[["popt"]], 126.51602)
  expect_equal(reg_data[["iik"]], 663.89465)
})
