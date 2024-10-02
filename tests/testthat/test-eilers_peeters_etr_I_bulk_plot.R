test_that("test-eilers_peeters_etr_I_bulk_plot", {
  test_data_file <- file.path(getwd(), "data", "bulk")
  csv_files <- list.files(test_data_file, pattern = ".csv", full.names = TRUE)

  expect_no_warning({
    pdf("test-eilers_peeters_etr_I_bulk_plot.pdf")

    for (file in csv_files) {
      try({
        title <- basename(file)
        data <- read_pam_data(file)
        model_result <- generate_regression_eilers_peeters_ETR_I(data)
        print(plot_control_eilers_peeters(
          data,
          model_result,
          etr_I_type,
          paste("eilers_peeters ETR I", title)
        ))
      })
    }

    dev.off()
  })
})
