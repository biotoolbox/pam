test_that("test platt ETR II 20231122_01_W3_T20_HL.csv", {
  test_data_file <- file.path(getwd(), "data", "20231122_01_W3_T20_HL.csv")
  data <- read_pam_data(test_data_file)
  reg_data <- generate_regression_platt_ETR_II(data)

  expect_equal(reg_data[["sdiff"]], 4.4844798)
  expect_equal(reg_data[["alpha"]], 0.1028022)
  expect_equal(reg_data[["beta"]], 0.02631246, tolerance = 0.000001)
  expect_equal(reg_data[["etrmpot"]], 58.592426)
  expect_equal(reg_data[["etr_max"]], 31.049374)
  expect_equal(reg_data[["ik"]], 302.03024)
})
