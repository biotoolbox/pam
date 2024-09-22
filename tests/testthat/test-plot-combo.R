test_that("plot combo ETR I", {
  test_data_file <- file.path(getwd(), "data", "20231122_10_W3_T20_ML.csv")
  message("using test data file: ", test_data_file)

  data <- read_pam_data(test_data_file)

  # print(plot_combo_etr_I("plot combo etr I / 20231122_10_W3_T20_ML.csv", data))
  # expect_equal(1, 1)
})
