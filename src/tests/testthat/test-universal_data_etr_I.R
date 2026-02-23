test_that("test-universal_data_etr_I - linux", {
  skip_if_not(is_debian_or_ubuntu())

  test_data_file <- testthat::test_path("data", "universal_data.csv")
  data <- read_universal_data(test_data_file)

  eilers_peeters <- eilers_peeters_modified(eilers_peeters_generate_regression_ETR_I(data))
  platt <- platt_modified(platt_generate_regression_ETR_I(data))
  walsby <- walsby_modified(walsby_generate_regression_ETR_I(data))
  vollenweider <- vollenweider_modified(vollenweider_generate_regression_ETR_I(data))

  expect_equal(eilers_peeters[["residual_sum_of_squares"]], 53.4454062)
  # expect_equal(eilers_peeters[["a"]], 0.000001479)
  # expect_equal(eilers_peeters[["b"]], 0.002444096)
  expect_equal(eilers_peeters[["c"]], 2.898407831)
  expect_equal(eilers_peeters[["d"]], NA_real_)
  expect_equal(eilers_peeters[["alpha"]], 0.345017009)
  expect_equal(eilers_peeters[["beta"]], NA_real_)
  expect_equal(eilers_peeters[["etrmax_with_photoinhibition"]], 151.851986)
  expect_equal(eilers_peeters[["etrmax_without_photoinhibition"]], NA_real_)
  expect_equal(eilers_peeters[["ik_with_photoinhibition"]], 440.1289906)
  expect_equal(eilers_peeters[["ik_without_photoinhibition"]], NA_real_)
  expect_equal(eilers_peeters[["im_with_photoinhibition"]], 1399.769662)
  expect_equal(eilers_peeters[["w"]], 1.18036237)
  expect_equal(eilers_peeters[["ib"]], NA_real_)
  expect_equal(eilers_peeters[["etrmax_with_without_ratio"]], NA_real_)

  expect_equal(platt[["residual_sum_of_squares"]], 55.4812913)
  expect_equal(platt[["a"]], 242.02858)
  expect_equal(platt[["b"]], 0.350792446)
  expect_equal(platt[["c"]], 0.056258718)
  expect_equal(platt[["d"]], NA_real_)
  expect_equal(platt[["alpha"]], 0.350792446)
  expect_equal(platt[["beta"]], 0.056258718)
  expect_equal(platt[["etrmax_with_photoinhibition"]], 151.8557330)
  expect_equal(platt[["etrmax_without_photoinhibition"]], 242.02858)
  expect_equal(platt[["ik_with_photoinhibition"]], 432.893395)
  expect_equal(platt[["ik_without_photoinhibition"]], 689.94810)
  expect_equal(platt[["im_with_photoinhibition"]], 1365.39202)
  expect_equal(platt[["w"]], NA_real_)
  expect_equal(platt[["ib"]], 4302.0636)
  expect_equal(platt[["etrmax_with_without_ratio"]], 1.59380602)

  expect_equal(walsby[["residual_sum_of_squares"]], 55.5823146)
  expect_equal(walsby[["a"]], 221.237830)
  expect_equal(walsby[["b"]], 0.387249932)
  expect_equal(walsby[["c"]], -0.035964258)
  expect_equal(walsby[["d"]], NA_real_)
  expect_equal(walsby[["alpha"]], 0.387249932)
  expect_equal(walsby[["beta"]], -0.035964258)
  expect_equal(walsby[["etrmax_with_photoinhibition"]], 151.8614464)
  expect_equal(walsby[["etrmax_without_photoinhibition"]], 221.237830)
  expect_equal(walsby[["ik_with_photoinhibition"]], 392.15358)
  expect_equal(walsby[["ik_without_photoinhibition"]], 571.305017)
  expect_equal(walsby[["im_with_photoinhibition"]], 1358.0)
  expect_equal(walsby[["w"]], NA_real_)
  expect_equal(walsby[["ib"]], NA_real_)
  expect_equal(walsby[["etrmax_with_without_ratio"]], 1.456840002)

  expect_equal(vollenweider[["residual_sum_of_squares"]], 55.4621712)
  expect_equal(vollenweider[["a"]], 191.5243754)
  # expect_equal(vollenweider[["b"]], 0.001864811)
  # expect_equal(vollenweider[["c"]], 0.00001772)
  expect_equal(vollenweider[["d"]], 0.067483929)
  expect_equal(vollenweider[["alpha"]], 0.31224738)
  expect_equal(vollenweider[["beta"]], NA_real_)
  expect_equal(vollenweider[["etrmax_with_photoinhibition"]], 152.615751)
  expect_equal(vollenweider[["etrmax_without_photoinhibition"]], 191.5243754)
  expect_equal(vollenweider[["ik_with_photoinhibition"]], 488.76551)
  expect_equal(vollenweider[["ik_without_photoinhibition"]], 613.37384)
  expect_equal(vollenweider[["im_with_photoinhibition"]], 1420.0)
  expect_equal(vollenweider[["w"]], NA_real_)
  expect_equal(vollenweider[["ib"]], NA_real_)
  expect_equal(vollenweider[["etrmax_with_without_ratio"]], 1.254945)
})

test_that("test-universal_data_etr_I", {
  test_data_file <- testthat::test_path("data", "universal_data.csv")
  data <- read_universal_data(test_data_file)

  eilers_peeters <- eilers_peeters_modified(eilers_peeters_generate_regression_ETR_I(data))
  expect_no_error(validate_model_result(eilers_peeters))

  platt <- platt_modified(platt_generate_regression_ETR_I(data))
  expect_no_error(validate_model_result(platt))

  walsby <- walsby_modified(walsby_generate_regression_ETR_I(data))
  expect_no_error(validate_model_result(walsby))

  vollenweider <- vollenweider_modified(vollenweider_generate_regression_ETR_I(data))
  expect_no_error(validate_model_result(vollenweider))
})

test_that("test-universal_data_etr_I plot", {
  test_data_file <- testthat::test_path("data", "universal_data.csv")
  data <- read_universal_data(test_data_file)

  eilers_peeters <- eilers_peeters_modified(eilers_peeters_generate_regression_ETR_I(data))
  platt <- platt_modified(platt_generate_regression_ETR_I(data))
  walsby <- walsby_modified(walsby_generate_regression_ETR_I(data))
  vollenweider <- vollenweider_modified(vollenweider_generate_regression_ETR_I(data))

  plot <- combo_plot_control(
    "test-intermediate-table_etr_I.csv",
    data,
    list(eilers_peeters, platt, walsby, vollenweider),
    list("eilers_peeters", "platt", "walsby", "vollenweider"),
    list(color_eilers_peeters, color_platt, color_walsby, color_vollenweider)
  )
  expect_s3_class(plot, "ggplot")
  expect_gt(length(plot$layers), 0)

  out <- file.path("results", "test-intermediate-table_etr_I.jpg")
  ggplot2::ggsave(out, create.dir = TRUE, plot = plot, units = "px", width = 1000, height = 1600, dpi = 100, limitsize = FALSE)
  expect_true(file.exists(out))
})
