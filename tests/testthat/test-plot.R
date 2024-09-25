test_that("plot combo ETR II", {
  test_data_file <- file.path(getwd(), "data", "bulk")
  csv_files <- list.files(test_data_file, pattern = ".csv", full.names = TRUE)
  # wrapper_pdf_eilers_peeters(test_data_file, "kek.pdf", etr_I_type)
  # wrapper_pdf_walsby(test_data_file, "kek2.pdf", etr_II_type)
  # wrapper_pdf_vollenweider(test_data_file, "kek2.pdf", etr_I_type)
  # data <- read_pam_data(test_data_file)
  # reg_data <- generate_regression_eilers_peeters_ETR_I(data)
  # print(plot_control_eilers_peeters(data, reg_data, "20231122_01_W3_T20_HL.csv", etr_I_type))
  pdf("")
  for (file in csv_files) {
    title <- basename(file)
    data <- read_pam_data(file)
    try({
      print(plot_combo_etr_II(title, data))
    })
  }
  dev.off()
})
