test_that("test vollenweider ETR II", {
  test_data_file <- file.path(getwd(), "data", "20231122_01_W3_T20_HL.csv")
  data <- read_pam_data(test_data_file)
  reg_data <- generate_regression_vollenweider_ETR_II(data)

  expect_equal(reg_data[["sdiff"]], 3.04234525)
  expect_equal(reg_data[["pmax"]], 36.644038)
  expect_equal(reg_data[["a"]], 0.00243709, tolerance = 0.00001)
  expect_equal(reg_data[["alpha"]], -0.00001386, tolerance = 0.0001)
  expect_equal(reg_data[["n"]], 1059.2046)
  expect_equal(reg_data[["ik"]], 410.32484)
  expect_equal(reg_data[["popt"]], 30.70697)
  expect_equal(reg_data[["iik"]], 343.844)
})
