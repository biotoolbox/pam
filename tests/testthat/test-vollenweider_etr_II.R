test_that("test vollenweider ETR II", {
  test_data_file <- file.path(getwd(), "data", "20231122_10_W3_T20_ML.csv")
  message("using test data file: ", test_data_file)

  data <- read_pam_data(test_data_file)
  data_vollenweider_etr_II <- generate_regression_vollenweider_ETR_II(data)
  # View(data_vollenweider_etr_II)

  # expect_equal(1, 1)
})
