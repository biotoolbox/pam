test_that("test-platt_control_plot_pdf_bulk_ETR_II", {
  test_data_dir <- file.path(getwd(), "data", "bulk")

  expect_no_warning({
    platt_control_plot_bulk_pdf_ETR_II(
      test_data_dir,
      "test-platt_control_plot_pdf_bulk_ETR_II.pdf"
    )
  })
})
