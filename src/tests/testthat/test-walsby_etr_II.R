test_that("test-walsby_etr_II generate regression 20240925.csv - linux", {
  skip_if_not(is_debian_or_ubuntu())

  test_data_file <- testthat::test_path("data", "20240925.csv")
  data <- read_dual_pam_data(test_data_file)
  model_result <- walsby_generate_regression_ETR_II(data)

  expect_equal(model_result[["residual_sum_of_squares"]], 13.3218134)
  expect_equal(model_result[["etr_max"]], 64.620494)
  expect_equal(model_result[["alpha"]], 0.197966245)
  expect_equal(model_result[["beta"]], -0.018109015)
})

test_that("test-walsby_etr_II generate regression 20240925.csv", {
  test_data_file <- testthat::test_path("data", "20240925.csv")
  data <- read_dual_pam_data(test_data_file)
  model_result <- walsby_generate_regression_ETR_II(data)
  expect_no_error(validate_model_result(model_result))
})

test_that("test-walsby_etr_II control plot 20240925.csv", {
  test_data_file <- testthat::test_path("data", "20240925.csv")
  data <- read_dual_pam_data(test_data_file)
  model_result <- walsby_generate_regression_ETR_II(data)

  plot <- plot_control(
    data,
    model_result,
    "walsby ETR II 20240925.csv",
    color_walsby
  )
  expect_s3_class(plot, "ggplot")
  expect_gt(length(plot$layers), 0)

  out <- file.path("results", "test-walsby_etr_II control plot 20240925.jpg")
  ggplot2::ggsave(out, create.dir = TRUE, plot = plot, units = "px", width = 1000, height = 1000, dpi = 100, limitsize = FALSE)
  expect_true(file.exists(out))
})

test_that("test-walsby_etr_II generate regression modified 20240925.csv - linux", {
  skip_if_not(is_debian_or_ubuntu())

  test_data_file <- testthat::test_path("data", "20240925.csv")
  data <- read_dual_pam_data(test_data_file)
  model_result <- walsby_generate_regression_ETR_II(data)
  model_result <- walsby_modified(model_result)

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
})

test_that("test-walsby_etr_II generate regression modified 20240925.csv", {
  test_data_file <- testthat::test_path("data", "20240925.csv")
  data <- read_dual_pam_data(test_data_file)

  model_result <- walsby_generate_regression_ETR_II(data)
  expect_no_error(validate_model_result(model_result))

  model_result <- walsby_modified(model_result)
  expect_no_error(validate_modified_model_result(model_result))
})

test_that("test-walsby_etr_II modified control plot 20240925.csv", {
  test_data_file <- testthat::test_path("data", "20240925.csv")
  data <- read_dual_pam_data(test_data_file)
  model_result <- walsby_generate_regression_ETR_II(data)
  model_result <- walsby_modified(model_result)

  plot <- plot_control(
    data,
    model_result,
    "walsby ETR II modified 20240925.csv",
    color_walsby
  )
  expect_s3_class(plot, "ggplot")
  expect_gt(length(plot$layers), 0)

  out <- file.path("results", "test-walsby_etr_II modified control plot 20240925.jpg")
  ggplot2::ggsave(out, create.dir = TRUE, plot = plot, units = "px", width = 1000, height = 1000, dpi = 100, limitsize = FALSE)
  expect_true(file.exists(out))
})
