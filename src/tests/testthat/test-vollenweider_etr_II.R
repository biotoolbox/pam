test_that("test-vollenweider_etr_II generate regression 20240925.csv", {
  test_data_file <- file.path(getwd(), "data", "20240925.csv")
  data <- read_dual_pam_data(test_data_file)
  model_result <- vollenweider_generate_regression_ETR_II(data)

  if (is_supported_os() == FALSE) {
    skip("Unsupported operating system for this test.")
  }

  if (is_debian_or_ubuntu()) {
    expect_equal(model_result[["residual_sum_of_squares"]], 3.0153935)
    expect_equal(model_result[["pmax"]], 52.752059)
    # expect_equal(model_result[["a"]], 0.002845961)
    # expect_equal(model_result[["alpha"]], -0.000378034)
    expect_equal(model_result[["n"]], 47.8354111)
    expect_equal(model_result[["ik"]], 345.86949)
    expect_equal(model_result[["popt"]], 44.1785457)
    expect_equal(model_result[["iik"]], 289.657149)
    expect_equal(model_result[["pmax_popt_and_ik_iik_ratio"]], 1.194065095)
  } else if (is_windows()) {
    expect_equal(model_result[["residual_sum_of_squares"]], 3.01641889)
    expect_equal(model_result[["pmax"]], 52.751066)
    # expect_equal(model_result[["a"]], 0.002845961)
    # expect_equal(model_result[["alpha"]], -0.000378034)
    expect_equal(model_result[["n"]], 49.592988)
    expect_equal(model_result[["ik"]], 345.86119)
    expect_equal(model_result[["popt"]], 44.1788258)
    expect_equal(model_result[["iik"]], 289.65748)
    expect_equal(model_result[["pmax_popt_and_ik_iik_ratio"]], 1.194035044)
  }
})

test_that("test-vollenweider_etr_II control plot 20240925.csv", {
  test_data_file <- file.path(getwd(), "data", "20240925.csv")
  data <- read_dual_pam_data(test_data_file)
  model_result <- vollenweider_generate_regression_ETR_II(data)

  expect_no_warning({
    plot <- plot_control(
      data,
      model_result,
      "vollenweider ETR II 20240925.csv",
      color_vollenweider
    )
    ggplot2::ggsave("results/test-vollenweider_etr_II control plot 20240925.jpg", create.dir = TRUE, plot = plot, units = "px", width = 1000, height = 1000, dpi = 100, limitsize = FALSE)
  })
})

test_that("test-vollenweider_etr_II generate regression modified 20240925.csv", {
  test_data_file <- file.path(getwd(), "data", "20240925.csv")
  data <- read_dual_pam_data(test_data_file)
  model_result <- vollenweider_generate_regression_ETR_II(data)
  model_result <- vollenweider_modified(model_result)

  if (is_supported_os() == FALSE) {
    skip("Unsupported operating system for this test.")
  }

  if (is_debian_or_ubuntu()) {
    expect_equal(model_result[["residual_sum_of_squares"]], 3.0153935)
    expect_equal(model_result[["a"]], 52.752059)
    # expect_equal(model_result[["b"]], 0.002845961)
    # expect_equal(model_result[["c"]], -0.000378034)
    expect_equal(model_result[["d"]], 47.8354111)
    expect_equal(model_result[["alpha"]], 0.15252013)
    expect_equal(model_result[["beta"]], NA_real_)
    expect_equal(model_result[["etrmax_with_photoinhibition"]], 44.1785457)
    expect_equal(model_result[["etrmax_without_photoinhibition"]], 52.752059)
    expect_equal(model_result[["ik_with_photoinhibition"]], 289.657149)
    expect_equal(model_result[["ik_without_photoinhibition"]], 345.86949)
    expect_equal(model_result[["im_with_photoinhibition"]], 767.0)
    expect_equal(model_result[["w"]], NA_real_)
    expect_equal(model_result[["ib"]], NA_real_)
    expect_equal(model_result[["etrmax_with_without_ratio"]], 1.194065095)
  } else if (is_windows()) {
    expect_equal(model_result[["residual_sum_of_squares"]], 3.01641889)
    expect_equal(model_result[["a"]], 52.751066)
    # expect_equal(model_result[["b"]], 0.002845961)
    # expect_equal(model_result[["c"]], -0.000378034)
    expect_equal(model_result[["d"]], 49.592988)
    expect_equal(model_result[["alpha"]], 0.15252092)
    expect_equal(model_result[["beta"]], NA_real_)
    expect_equal(model_result[["etrmax_with_photoinhibition"]], 44.1788258)
    expect_equal(model_result[["etrmax_without_photoinhibition"]], 52.751066)
    expect_equal(model_result[["ik_with_photoinhibition"]], 289.65748)
    expect_equal(model_result[["ik_without_photoinhibition"]], 345.86119)
    expect_equal(model_result[["im_with_photoinhibition"]], 767.0)
    expect_equal(model_result[["w"]], NA_real_)
    expect_equal(model_result[["ib"]], NA_real_)
    expect_equal(model_result[["etrmax_with_without_ratio"]], 1.194035044)
  }
})

test_that("test-vollenweider_etr_II modified control plot 20240925.csv", {
  test_data_file <- file.path(getwd(), "data", "20240925.csv")
  data <- read_dual_pam_data(test_data_file)
  model_result <- vollenweider_generate_regression_ETR_II(data)
  model_result <- vollenweider_modified(model_result)

  expect_no_warning({
    plot <- plot_control(
      data,
      model_result,
      "vollenweider ETR II modified 20240925.csv",
      color_vollenweider
    )
    ggplot2::ggsave("results/test-vollenweider_etr_II modified control plot 20240925.jpg", create.dir = TRUE, plot = plot, units = "px", width = 1000, height = 1000, dpi = 100, limitsize = FALSE)
  })
})
