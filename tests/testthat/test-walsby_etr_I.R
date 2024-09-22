test_that("test walsby ETR I", {
  test_data_file <- file.path(getwd(), "data", "20231122_10_W3_T20_ML.csv")
  message("using test data file: ", test_data_file)

  data <- read_pam_data(test_data_file)
  data_walsby_etr_I <- generate_regression_walsby_ETR_I(data)
  # View(data_walsby_etr_I)

  # expect_equal(1, 1)
})
