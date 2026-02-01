test_that("test-walsby_etr_I generate regression 20240925.csv", {
  test_data_file <- file.path(getwd(), "data", "20240925.csv")
  data <- read_dual_pam_data(test_data_file)
  model_result <- walsby_generate_regression_ETR_I(data)

  if (is_supported_os() == FALSE) {
    skip("Unsupported operating system for this test.")
  }

  if (is_debian_or_ubuntu()) {
    expect_equal(model_result[["residual_sum_of_squares"]], 55.5823146)
    expect_equal(model_result[["etr_max"]], 221.237830)
    expect_equal(model_result[["alpha"]], 0.387249932)
    expect_equal(model_result[["beta"]], -0.035964258)
  } else if (is_windows()) {
    expect_equal(model_result[["residual_sum_of_squares"]], 55.5823146)
    expect_equal(model_result[["etr_max"]], 221.237850)
    expect_equal(model_result[["alpha"]], 0.387249941)
    expect_equal(model_result[["beta"]], -0.035964269)
  }
})

test_that("test-walsby_etr_I control plot 20240925.csv", {
  test_data_file <- file.path(getwd(), "data", "20240925.csv")
  data <- read_dual_pam_data(test_data_file)
  model_result <- walsby_generate_regression_ETR_I(data)

  expect_no_warning({
    plot <- plot_control(
      data,
      model_result,
      "walsby ETR I 20240925.csv",
      color_platt
    )

    ggplot2::ggsave("results/test-walsby_etr_I control plot 20240925.jpg", create.dir = TRUE, plot = plot, units = "px", width = 1000, height = 1000, dpi = 100, limitsize = FALSE)
  })
})

test_that("test-walsby_etr_I generate regression modified 20240925.csv", {
  test_data_file <- file.path(getwd(), "data", "20240925.csv")
  data <- read_dual_pam_data(test_data_file)
  model_result <- walsby_generate_regression_ETR_I(data)
  model_result <- walsby_modified(model_result)

  if (is_supported_os() == FALSE) {
    skip("Unsupported operating system for this test.")
  }

  if (is_debian_or_ubuntu()) {
    expect_equal(model_result[["residual_sum_of_squares"]], 55.5823146)
    expect_equal(model_result[["a"]], 221.237830)
    expect_equal(model_result[["b"]], 0.387249932)
    expect_equal(model_result[["c"]], -0.035964258)
    expect_equal(model_result[["d"]], NA_real_)
    expect_equal(model_result[["alpha"]], 0.387249932)
    expect_equal(model_result[["beta"]], -0.035964258)
    expect_equal(model_result[["etrmax_with_photoinhibition"]], 151.8614464)
    expect_equal(model_result[["etrmax_without_photoinhibition"]], 221.237830)
    expect_equal(model_result[["ik_with_photoinhibition"]], 392.15358)
    expect_equal(model_result[["ik_without_photoinhibition"]], 571.305017)
    expect_equal(model_result[["im_with_photoinhibition"]], 1358.0)
    expect_equal(model_result[["w"]], NA_real_)
    expect_equal(model_result[["ib"]], NA_real_)
    expect_equal(model_result[["etrmax_with_without_ratio"]], 1.456840002)
  } else if (is_windows()) {
    expect_equal(model_result[["residual_sum_of_squares"]], 55.5823146)
    expect_equal(model_result[["a"]], 221.237850)
    expect_equal(model_result[["b"]], 0.387249941)
    expect_equal(model_result[["c"]], -0.035964269)
    expect_equal(model_result[["d"]], NA_real_)
    expect_equal(model_result[["alpha"]], 0.387249941)
    expect_equal(model_result[["beta"]], -0.035964269)
    expect_equal(model_result[["etrmax_with_photoinhibition"]], 151.8614464)
    expect_equal(model_result[["etrmax_without_photoinhibition"]], 221.237850)
    expect_equal(model_result[["ik_with_photoinhibition"]], 392.153567)
    expect_equal(model_result[["ik_without_photoinhibition"]], 571.305058)
    expect_equal(model_result[["im_with_photoinhibition"]], 1358.0)
    expect_equal(model_result[["w"]], NA_real_)
    expect_equal(model_result[["ib"]], NA_real_)
    expect_equal(model_result[["etrmax_with_without_ratio"]], 1.45684014)
  }
})

test_that("test-walsby_etr_I modified control plot 20240925.csv", {
  test_data_file <- file.path(getwd(), "data", "20240925.csv")
  data <- read_dual_pam_data(test_data_file)
  model_result <- walsby_generate_regression_ETR_I(data)
  model_result <- walsby_modified(model_result)

  expect_no_warning({
    plot <- plot_control(
      data,
      model_result,
      "walsby ETR I modified 20240925.csv",
      color_platt
    )
    ggplot2::ggsave("results/test-walsby_etr_I modified control plot 20240925.jpg", create.dir = TRUE, plot = plot, units = "px", width = 1000, height = 1000, dpi = 100, limitsize = FALSE)
  })
})
