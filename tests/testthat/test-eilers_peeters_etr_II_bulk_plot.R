test_that("test-eilers_peeters_etr_II_bulk_plot", {
  test_data_file <- file.path(getwd(), "data", "bulk")
  csv_files <- list.files(test_data_file, pattern = ".csv", full.names = TRUE)

  pdf("test-eilers_peeters_etr_II_bulk_plot.pdf")
  for (file in csv_files) {
    title <- basename(file)
    data <- read_pam_data(file)
    reg_data <- generate_regression_eilers_peeters_ETR_II(data)
    print(plot_control_eilers_peeters(
      data,
      reg_data,
      paste("eilers_peeters ETR II", title),
      etr_II_type
    ))
  }
  dev.off()
})
