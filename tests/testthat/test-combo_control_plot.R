test_that("test-combo_control_plot 20231122_01_W3_T20_HL.csv", {
  test_data_file <- file.path(getwd(), "data", "20231122_01_W3_T20_HL.csv")

  expect_no_error({
    data <- read_dual_pam_data(test_data_file)

    eilers_peeters <- eilers_peeters_modified(eilers_peeters_generate_regression_ETR_II(data))
    platt <- platt_modified(platt_generate_regression_ETR_II(data))
    walsby <- walsby_modified(walsby_generate_regression_ETR_II(data))
    vollenweider <- vollenweider_modified(vollenweider_generate_regression_ETR_II(data))

    plot <- combo_control_plot(
      "test-combo_control_plot_20231122_01_W3_T20_HL.csv",
      data,
      list(eilers_peeters, platt, walsby, vollenweider),
      list("eilers_peeters", "platt", "walsby", "vollenweider"),
      list(color_eilers_peeters, color_platt, color_walsby, color_vollenweider)
    )

    ggsave("test_combo_control_plot.jpg", plot = plot, units = "px", width = 1000, height = 2000, dpi = 100, limitsize = FALSE)
  })
})
