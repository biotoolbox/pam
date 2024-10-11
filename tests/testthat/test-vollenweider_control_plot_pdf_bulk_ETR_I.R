test_that("test-vollenweider_control_plot_pdf_bulk_ETR_I", {
  test_data_dir <- file.path(getwd(), "data", "bulk")

  expect_no_warning({
    vollenweider_control_plot_bulk_pdf_ETR_I(
      test_data_dir,
      "test-vollenweider_control_plot_pdf_bulk_ETR_I.pdf"
    )
  })
})
