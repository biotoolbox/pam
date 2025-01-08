test_that("test-combo_plot_control 20240925.csv", {
  test_data_file <- file.path(getwd(), "data", "20240925.csv")

  expect_no_error({
    data <- read_dual_pam_data(test_data_file)

    eilers_peeters <- eilers_peeters_modified(eilers_peeters_generate_regression_ETR_I(data))
    platt <- platt_modified(platt_generate_regression_ETR_I(data))
    walsby <- walsby_modified(walsby_generate_regression_ETR_I(data))
    vollenweider <- vollenweider_modified(vollenweider_generate_regression_ETR_I(data))

    plot <- combo_plot_control(
      "etr I test-combo_plot_control_20240925.csv",
      data,
      list(eilers_peeters, platt, walsby, vollenweider),
      list("eilers_peeters", "platt", "walsby", "vollenweider"),
      list(color_eilers_peeters, color_platt, color_walsby, color_vollenweider)
    )
    
    ggplot2::ggsave("results/test_combo_plot_control.jpg", create.dir = TRUE, plot = plot, units = "px", width = 1000, height = 1600, dpi = 100, limitsize = FALSE)
  })
})
