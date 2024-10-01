test_that("test walsby ETR II", {
  test_data_file <- file.path(getwd(), "data", "20231122_01_W3_T20_HL.csv")
  data <- read_pam_data(test_data_file)
  reg_data <- generate_regression_walsby_ETR_II(data)

  expect_equal(reg_data[["sdiff"]], 4.4596755)
  expect_equal(reg_data[["etr_max"]], 47.999349)
  expect_equal(reg_data[["alpha"]], 0.11605834)
  expect_equal(reg_data[["beta"]], -0.012745, tolerance = 0.000001)
})
