test_that("test-eilers_peeters_control_plot_pdf_bulk_ETR_I", {
  test_data_dir <- file.path(getwd(), "data", "bulk")

  expect_no_warning({
    eilers_peeters_control_plot_bulk_pdf_ETR_I(
      test_data_dir,
      "test-eilers_peeters_control_plot_pdf_bulk_ETR_I.pdf"
    )
  })
})
