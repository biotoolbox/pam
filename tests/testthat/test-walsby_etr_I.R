test_that("test walsby ETR I", {
  test_data_file <- file.path(getwd(), "data", "20231122_01_W3_T20_HL.csv")
  data <- read_pam_data(test_data_file)
  reg_data <- generate_regression_walsby_ETR_I(data)

  expect_equal(reg_data[["sdiff"]], 1247.65184)
  expect_equal(reg_data[["etr_max"]], 3970930.3)
  expect_equal(reg_data[["alpha"]], 27.690236)
  expect_equal(reg_data[["beta"]], -27.4705392)
})
