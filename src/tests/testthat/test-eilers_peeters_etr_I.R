test_that("test-eilers_peeters_etr_I generate regression 20240925.csv", {
  test_data_file <- file.path(getwd(), "data", "20240925.csv")
  data <- read_dual_pam_data(test_data_file)
  model_result <- eilers_peeters_generate_regression_ETR_I(data)

  if (is_supported_os() == FALSE) {
    skip("Unsupported operating system for this test.")
  }

  if (is_debian_or_ubuntu()) {
    expect_equal(model_result[["residual_sum_of_squares"]], 53.4454062)
    # expect_equal(model_result[["a"]], 0.000001479)
    # expect_equal(model_result[["b"]], 0.002444096)
    expect_equal(model_result[["c"]], 2.898407831)
    expect_equal(model_result[["pm"]], 151.851986)
    expect_equal(model_result[["s"]], 0.345017009)
    expect_equal(model_result[["ik"]], 440.1289906)
    expect_equal(model_result[["im"]], 1399.769662)
    expect_equal(model_result[["w"]], 1.18036237)
  } else if (is_windows()) {
    expect_equal(model_result[["residual_sum_of_squares"]], 53.4454062)
    # expect_equal(model_result[["a"]], 0.000001479)
    # expect_equal(model_result[["b"]], 0.002444096)
    expect_equal(model_result[["c"]], 2.898407831)
    expect_equal(model_result[["pm"]], 151.851986)
    expect_equal(model_result[["s"]], 0.345017009)
    expect_equal(model_result[["ik"]], 440.1289906)
    expect_equal(model_result[["im"]], 1399.769662)
    expect_equal(model_result[["w"]], 1.18036237)
  }
})

test_that("test-eilers_peeters_etr_I control plot 20240925.csv", {
  test_data_file <- file.path(getwd(), "data", "20240925.csv")
  data <- read_dual_pam_data(test_data_file)
  model_result <- eilers_peeters_generate_regression_ETR_I(data)

  expect_no_warning({
    plot <- plot_control(
      data,
      model_result,
      "eilers_peeters ETR I 20240925.csv",
      color_eilers_peeters
    )
    ggplot2::ggsave("results/test-eilers_peeters_etr_I control plot 20240925.jpg", create.dir = TRUE, plot = plot, units = "px", width = 1000, height = 1000, dpi = 100, limitsize = FALSE)
  })
})

test_that("test-eilers_peeters_etr_I generate regression modified 20240925.csv", {
  test_data_file <- file.path(getwd(), "data", "20240925.csv")
  data <- read_dual_pam_data(test_data_file)
  model_result <- eilers_peeters_generate_regression_ETR_I(data)

  expect_no_warning(
    model_result <- eilers_peeters_modified(model_result)
  )

  if (is_supported_os() == FALSE) {
    skip("Unsupported operating system for this test.")
  }

  if (is_debian_or_ubuntu()) {
    expect_equal(model_result[["residual_sum_of_squares"]], 53.4454062)
    # expect_equal(model_result[["a"]], 0.000001479)
    # expect_equal(model_result[["b"]], 0.002444096)
    expect_equal(model_result[["c"]], 2.898407831)
    expect_equal(model_result[["d"]], NA_real_)
    expect_equal(model_result[["alpha"]], 0.345017009)
    expect_equal(model_result[["beta"]], NA_real_)
    expect_equal(model_result[["etrmax_with_photoinhibition"]], 151.851986)
    expect_equal(model_result[["etrmax_without_photoinhibition"]], NA_real_)
    expect_equal(model_result[["ik_with_photoinhibition"]], 440.1289906)
    expect_equal(model_result[["ik_without_photoinhibition"]], NA_real_)
    expect_equal(model_result[["im_with_photoinhibition"]], 1399.769662)
    expect_equal(model_result[["w"]], 1.18036237)
    expect_equal(model_result[["ib"]], NA_real_)
    expect_equal(model_result[["etrmax_with_without_ratio"]], NA_real_)
  } else if (is_windows()) {
    expect_equal(model_result[["residual_sum_of_squares"]], 53.4454062)
    # expect_equal(model_result[["a"]], 0.000001479)
    # expect_equal(model_result[["b"]], 0.002444096)
    expect_equal(model_result[["c"]], 2.898407831)
    expect_equal(model_result[["d"]], NA_real_)
    expect_equal(model_result[["alpha"]], 0.345017009)
    expect_equal(model_result[["beta"]], NA_real_)
    expect_equal(model_result[["etrmax_with_photoinhibition"]], 151.851986)
    expect_equal(model_result[["etrmax_without_photoinhibition"]], NA_real_)
    expect_equal(model_result[["ik_with_photoinhibition"]], 440.1289906)
    expect_equal(model_result[["ik_without_photoinhibition"]], NA_real_)
    expect_equal(model_result[["im_with_photoinhibition"]], 1399.769662)
    expect_equal(model_result[["w"]], 1.18036237)
    expect_equal(model_result[["ib"]], NA_real_)
    expect_equal(model_result[["etrmax_with_without_ratio"]], NA_real_)
  }
})

test_that("test-eilers_peeters_etr_I modified control plot 20240925.csv", {
  test_data_file <- file.path(getwd(), "data", "20240925.csv")
  data <- read_dual_pam_data(test_data_file)
  model_result <- eilers_peeters_generate_regression_ETR_I(data)
  model_result <- eilers_peeters_modified(model_result)

  expect_no_warning({
    plot <- plot_control(
      data,
      model_result,
      "eilers_peeters ETR I modified 20240925.csv",
      color_eilers_peeters
    )
    ggplot2::ggsave("results/test-eilers_peeters_etr_I modified control plot 20240925.jpg", create.dir = TRUE, plot = plot, units = "px", width = 1000, height = 1000, dpi = 100, limitsize = FALSE)
  })
})