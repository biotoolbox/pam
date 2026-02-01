test_that("test-platt_etr_I generate regression 20240925.csv", {
  test_data_file <- file.path(getwd(), "data", "20240925.csv")
  data <- read_dual_pam_data(test_data_file)
  model_result <- platt_generate_regression_ETR_I(data)

  if (is_supported_os() == FALSE) {
    skip("Unsupported operating system for this test.")
  }

  if (is_debian_or_ubuntu()) {
    expect_equal(model_result[["residual_sum_of_squares"]], 55.4812913)
    expect_equal(model_result[["alpha"]], 0.350792446)
    expect_equal(model_result[["beta"]], 0.056258718)
    expect_equal(model_result[["ps"]], 242.02858)
    expect_equal(model_result[["pm"]], 151.8557330)
    expect_equal(model_result[["ik"]], 432.893395)
    expect_equal(model_result[["is"]], 689.94810)
    expect_equal(model_result[["ib"]], 4302.0636)
    expect_equal(model_result[["im"]], 1365.39202)
  } else if (is_windows()) {
    expect_equal(model_result[["residual_sum_of_squares"]], 55.4812913)
    expect_equal(model_result[["alpha"]], 0.350792446)
    expect_equal(model_result[["beta"]], 0.056258718)
    expect_equal(model_result[["ps"]], 242.02858)
    expect_equal(model_result[["pm"]], 151.8557330)
    expect_equal(model_result[["ik"]], 432.893395)
    expect_equal(model_result[["is"]], 689.94810)
    expect_equal(model_result[["ib"]], 4302.0636)
    expect_equal(model_result[["im"]], 1365.39202)
  }
})

test_that("test-platt_etr_I generate regression modified 20240925.csv", {
  test_data_file <- file.path(getwd(), "data", "20240925.csv")
  data <- read_dual_pam_data(test_data_file)
  model_result <- platt_generate_regression_ETR_I(data)
  model_result <- platt_modified(model_result)

  if (is_supported_os() == FALSE) {
    skip("Unsupported operating system for this test.")
  }

  if (is_debian_or_ubuntu()) {
    expect_equal(model_result[["residual_sum_of_squares"]], 55.4812913)
    expect_equal(model_result[["a"]], 242.02858)
    expect_equal(model_result[["b"]], 0.350792446)
    expect_equal(model_result[["c"]], 0.056258718)
    expect_equal(model_result[["d"]], NA_real_)
    expect_equal(model_result[["alpha"]], 0.350792446)
    expect_equal(model_result[["beta"]], 0.056258718)
    expect_equal(model_result[["etrmax_with_photoinhibition"]], 151.8557330)
    expect_equal(model_result[["etrmax_without_photoinhibition"]], 242.02858)
    expect_equal(model_result[["ik_with_photoinhibition"]], 432.893395)
    expect_equal(model_result[["ik_without_photoinhibition"]], 689.94810)
    expect_equal(model_result[["im_with_photoinhibition"]], 1365.39202)
    expect_equal(model_result[["w"]], NA_real_)
    expect_equal(model_result[["ib"]], 4302.0636)
    expect_equal(model_result[["etrmax_with_without_ratio"]], 1.59380602)
  } else if (is_windows()) {
    expect_equal(model_result[["residual_sum_of_squares"]], 55.4812913)
    expect_equal(model_result[["a"]], 242.02858)
    expect_equal(model_result[["b"]], 0.350792446)
    expect_equal(model_result[["c"]], 0.056258718)
    expect_equal(model_result[["d"]], NA_real_)
    expect_equal(model_result[["alpha"]], 0.350792446)
    expect_equal(model_result[["beta"]], 0.056258718)
    expect_equal(model_result[["etrmax_with_photoinhibition"]], 151.8557330)
    expect_equal(model_result[["etrmax_without_photoinhibition"]], 242.02858)
    expect_equal(model_result[["ik_with_photoinhibition"]], 432.893395)
    expect_equal(model_result[["ik_without_photoinhibition"]], 689.94810)
    expect_equal(model_result[["im_with_photoinhibition"]], 1365.39202)
    expect_equal(model_result[["w"]], NA_real_)
    expect_equal(model_result[["ib"]], 4302.0636)
    expect_equal(model_result[["etrmax_with_without_ratio"]], 1.59380602)
  }
})

test_that("test-platt_etr_I modified control plot 20240925.csv", {
  test_data_file <- file.path(getwd(), "data", "20240925.csv")
  data <- read_dual_pam_data(test_data_file)
  model_result <- platt_generate_regression_ETR_I(data)
  model_result <- platt_modified(model_result)

  expect_no_warning({
    plot <- plot_control(
      data,
      model_result,
      "platt ETR I modified 20240925.csv",
      color_platt
    )
    ggplot2::ggsave("results/test-platt_etr_I modified control plot 20240925.jpg", create.dir = TRUE, plot = plot, units = "px", width = 1000, height = 1000, dpi = 100, limitsize = FALSE)
  })
})
