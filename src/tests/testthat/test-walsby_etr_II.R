test_that("test-walsby_etr_II generate regression 20240925.csv", {
  test_data_file <- file.path(getwd(), "data", "20240925.csv")
  data <- read_dual_pam_data(test_data_file)
  model_result <- walsby_generate_regression_ETR_II(data)

  if (is_supported_os() == FALSE) {
    skip("Unsupported operating system for this test.")
  }

  if (is_debian_or_ubuntu()) {
    expect_equal(model_result[["residual_sum_of_squares"]], 13.3218134)
    expect_equal(model_result[["etr_max"]], 64.620494)
    expect_equal(model_result[["alpha"]], 0.197966245)
    expect_equal(model_result[["beta"]], -0.018109015)
  } else if (is_windows()) {
    expect_equal(model_result[["residual_sum_of_squares"]], 13.3218134)
    expect_equal(model_result[["etr_max"]], 64.620494)
    expect_equal(model_result[["alpha"]], 0.197966245)
    expect_equal(model_result[["beta"]], -0.018109014)
  }
})

test_that("test-walsby_etr_II control plot 20240925.csv", {
  test_data_file <- file.path(getwd(), "data", "20240925.csv")
  data <- read_dual_pam_data(test_data_file)
  model_result <- walsby_generate_regression_ETR_II(data)

  expect_no_warning({
    plot <- plot_control(
      data,
      model_result,
      "walsby ETR II 20240925.csv",
      color_walsby
    )
    ggplot2::ggsave("results/test-walsby_etr_II control plot 20240925.jpg", create.dir = TRUE, plot = plot, units = "px", width = 1000, height = 1000, dpi = 100, limitsize = FALSE)
  })
})

test_that("test-walsby_etr_II generate regression modified 20240925.csv", {
  test_data_file <- file.path(getwd(), "data", "20240925.csv")
  data <- read_dual_pam_data(test_data_file)
  model_result <- walsby_generate_regression_ETR_II(data)
  model_result <- walsby_modified(model_result)

  if (is_supported_os() == FALSE) {
    skip("Unsupported operating system for this test.")
  }

  if (is_debian_or_ubuntu()) {
    expect_equal(model_result[["residual_sum_of_squares"]], 13.3218134)
    expect_equal(model_result[["a"]], 64.620494)
    expect_equal(model_result[["b"]], 0.197966245)
    expect_equal(model_result[["c"]], -0.018109015)
    expect_equal(model_result[["d"]], NA_real_)
    expect_equal(model_result[["alpha"]], 0.197966245)
    expect_equal(model_result[["beta"]], -0.018109015)
    expect_equal(model_result[["etrmax_with_photoinhibition"]], 44.5716318)
    expect_equal(model_result[["etrmax_without_photoinhibition"]], 64.620494)
    expect_equal(model_result[["ik_with_photoinhibition"]], 225.147635)
    expect_equal(model_result[["ik_without_photoinhibition"]], 326.42178)
    expect_equal(model_result[["im_with_photoinhibition"]], 781.0)
    expect_equal(model_result[["w"]], NA_real_)
    expect_equal(model_result[["ib"]], NA_real_)
    expect_equal(model_result[["etrmax_with_without_ratio"]], 1.44981217)
  } else if (is_windows()) {
    expect_equal(model_result[["residual_sum_of_squares"]], 13.3218134)
    expect_equal(model_result[["a"]], 64.620494)
    expect_equal(model_result[["b"]], 0.197966245)
    expect_equal(model_result[["c"]], -0.018109014)
    expect_equal(model_result[["d"]], NA_real_)
    expect_equal(model_result[["alpha"]], 0.197966245)
    expect_equal(model_result[["beta"]], -0.018109014)
    expect_equal(model_result[["etrmax_with_photoinhibition"]], 44.5716318)
    expect_equal(model_result[["etrmax_without_photoinhibition"]], 64.620494)
    expect_equal(model_result[["ik_with_photoinhibition"]], 225.147635)
    expect_equal(model_result[["ik_without_photoinhibition"]], 326.4217749)
    expect_equal(model_result[["im_with_photoinhibition"]], 781.0)
    expect_equal(model_result[["w"]], NA_real_)
    expect_equal(model_result[["ib"]], NA_real_)
    expect_equal(model_result[["etrmax_with_without_ratio"]], 1.449812147)
  }
})

test_that("test-walsby_etr_II modified control plot 20240925.csv", {
  test_data_file <- file.path(getwd(), "data", "20240925.csv")
  data <- read_dual_pam_data(test_data_file)
  model_result <- walsby_generate_regression_ETR_II(data)
  model_result <- walsby_modified(model_result)

  expect_no_warning({
    plot <- plot_control(
      data,
      model_result,
      "walsby ETR II modified 20240925.csv",
      color_walsby
    )
    ggplot2::ggsave("results/test-walsby_etr_II modified control plot 20240925.jpg", create.dir = TRUE, plot = plot, units = "px", width = 1000, height = 1000, dpi = 100, limitsize = FALSE)
  })
})
