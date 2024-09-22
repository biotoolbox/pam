test_that("plot combo ETR I", {
  test_data_file <- file.path(getwd(), "data", "bulk")
  wrapper_pdf_eilers_peeters(test_data_file, "kek.pdf", etr_I_type)
  # data <- read_pam_data(test_data_file)
  # reg_data <- generate_regression_eilers_peeters_ETR_I(data)
  # print(plot_control_eilers_peeters(data, reg_data, "20231122_01_W3_T20_HL.csv", etr_I_type))
})
