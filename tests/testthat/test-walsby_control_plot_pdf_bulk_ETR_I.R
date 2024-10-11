test_that("test-walsby_control_plot_pdf_bulk_ETR_I", {
  test_data_dir <- file.path(getwd(), "data", "bulk")

  expect_no_warning({
    walsby_control_plot_bulk_pdf_ETR_I(
      test_data_dir,
      "test-walsby_control_plot_pdf_bulk_ETR_I.pdf"
    )
  })
})
