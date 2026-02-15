test_that("test-combo_plot_control 20240925.csv", {
  test_data_file <- testthat::test_path("data", "20240925.csv")
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

  expect_s3_class(plot, "ggplot")
  expect_gt(length(plot$layers), 0)

  out <- file.path("results", "test_combo_plot_control_etr_I.jpg")
  ggplot2::ggsave(out, create.dir = TRUE, plot = plot, units = "px", width = 1000, height = 2100, dpi = 100, limitsize = FALSE)
  expect_true(file.exists(out))
})
