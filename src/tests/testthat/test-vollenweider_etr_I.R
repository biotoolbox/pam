test_that("test-vollenweider_etr_I generate regression 20240925.csv", {
  test_data_file <- file.path(getwd(), "data", "20240925.csv")
  data <- read_dual_pam_data(test_data_file)
  model_result <- vollenweider_generate_regression_ETR_I(data)

  if (is_supported_os() == FALSE) {
    skip("Unsupported operating system for this test.")
  }

  if (is_debian_or_ubuntu()) {
    expect_equal(model_result[["sdiff"]], 55.4621712)
    expect_equal(model_result[["pmax"]], 191.5243754)
    # expect_equal(model_result[["a"]], 0.001864811)
    # expect_equal(model_result[["alpha"]], 0.00001772)
    expect_equal(model_result[["n"]], 0.067483929)
    expect_equal(model_result[["ik"]], 613.37384)
    expect_equal(model_result[["popt"]], 152.615751)
    expect_equal(model_result[["iik"]], 488.76551)
    expect_equal(model_result[["pmax_popt_and_ik_iik_ratio"]], 1.254945)
  } else if (is_windows()) {
    expect_equal(model_result[["sdiff"]], 57.444503)
    expect_equal(model_result[["pmax"]], 165.46706)
    # expect_equal(model_result[["a"]], 0.001864811)
    # expect_equal(model_result[["alpha"]], 0.00001772)
    expect_equal(model_result[["n"]], 42.4595519)
    expect_equal(model_result[["ik"]], 536.24841)
    expect_equal(model_result[["popt"]], 153.100940)
    expect_equal(model_result[["iik"]], 496.172089)
    expect_equal(model_result[["pmax_popt_and_ik_iik_ratio"]], 1.0807710)
  }
})

test_that("test-vollenweider_etr_I control plot 20240925.csv", {
  test_data_file <- file.path(getwd(), "data", "20240925.csv")
  data <- read_dual_pam_data(test_data_file)
  model_result <- vollenweider_generate_regression_ETR_I(data)

  expect_no_warning(
    print(
      plot_control(
        data,
        model_result,
        "vollenweider ETR I 20240925.csv",
        color_vollenweider
      )
    )
  )
})

test_that("test-vollenweider_etr_I generate regression modified 20240925.csv", {
  test_data_file <- file.path(getwd(), "data", "20240925.csv")
  data <- read_dual_pam_data(test_data_file)
  model_result <- vollenweider_generate_regression_ETR_I(data)
  model_result <- vollenweider_modified(model_result)

  if (is_supported_os() == FALSE) {
    skip("Unsupported operating system for this test.")
  }

  if (is_debian_or_ubuntu()) {
    expect_equal(model_result[["sdiff"]], 55.4621712)
    expect_equal(model_result[["a"]], 191.5243754)
    # expect_equal(model_result[["b"]], 0.001864811)
    # expect_equal(model_result[["c"]], 0.00001772)
    expect_equal(model_result[["d"]], 0.067483929)
    expect_equal(model_result[["alpha"]], 0.31224738)
    expect_equal(model_result[["beta"]], NA_real_)
    expect_equal(model_result[["etrmax_with_photoinhibition"]], 152.615751)
    expect_equal(model_result[["etrmax_without_photoinhibition"]], 191.5243754)
    expect_equal(model_result[["ik_with_photoinhibition"]], 488.76551)
    expect_equal(model_result[["ik_without_photoinhibition"]], 613.37384)
    expect_equal(model_result[["im_with_photoinhibition"]], 1420.0)
    expect_equal(model_result[["w"]], NA_real_)
    expect_equal(model_result[["ib"]], NA_real_)
    expect_equal(model_result[["etrmax_with_without_ratio"]], 1.254945)
  } else if (is_windows()) {
    expect_equal(model_result[["sdiff"]], 57.444503)
    expect_equal(model_result[["a"]], 165.46706)
    # expect_equal(model_result[["b"]], 0.001864811)
    # expect_equal(model_result[["c"]], 0.00001772)
    expect_equal(model_result[["d"]], 42.4595519)
    expect_equal(model_result[["alpha"]], 0.30856419)
    expect_equal(model_result[["beta"]], NA_real_)
    expect_equal(model_result[["etrmax_with_photoinhibition"]], 153.100940)
    expect_equal(model_result[["etrmax_without_photoinhibition"]], 165.46706)
    expect_equal(model_result[["ik_with_photoinhibition"]], 496.172089)
    expect_equal(model_result[["ik_without_photoinhibition"]], 536.248419)
    expect_equal(model_result[["im_with_photoinhibition"]], 1420.0)
    expect_equal(model_result[["w"]], NA_real_)
    expect_equal(model_result[["ib"]], NA_real_)
    expect_equal(model_result[["etrmax_with_without_ratio"]], 1.0807710)
  }
})

test_that("test-vollenweider_etr_I modified control plot 20240925.csv", {
  test_data_file <- file.path(getwd(), "data", "20240925.csv")
  data <- read_dual_pam_data(test_data_file)
  model_result <- vollenweider_generate_regression_ETR_I(data)
  model_result <- vollenweider_modified(model_result)

  expect_no_warning(
    print(
      plot_control(
        data,
        model_result,
        "vollenweider ETR I modified 20240925.csv",
        color_vollenweider
      )
    )
  )
})
