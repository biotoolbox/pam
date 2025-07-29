test_that("test-walsby_etr_I generate regression 20240925.csv", {
  test_data_file <- file.path(getwd(), "data", "20240925.csv")
  data <- read_dual_pam_data(test_data_file)
  model_result <- walsby_generate_regression_ETR_I(data)

  if (is_supported_os() == FALSE) {
    skip("Unsupported operating system for this test.")
  }

  if (is_debian_or_ubuntu()) {
    expect_equal(model_result[["sdiff"]], 55.5823146)
    expect_equal(model_result[["etr_max"]], 221.23782)
    expect_equal(model_result[["alpha"]], 0.387249932)
    expect_equal(model_result[["beta"]], -0.035964253)
  } else if (is_windows()) {
    expect_equal(model_result[["sdiff"]], 55.5823146)
    expect_equal(model_result[["etr_max"]], 221.23782)
    expect_equal(model_result[["alpha"]], 0.387249932)
    expect_equal(model_result[["beta"]], -0.035964253)
  }
})

test_that("test-walsby_etr_I control plot 20240925.csv", {
  test_data_file <- file.path(getwd(), "data", "20240925.csv")
  data <- read_dual_pam_data(test_data_file)
  model_result <- walsby_generate_regression_ETR_I(data)

  expect_no_warning(
    print(
      plot_control(
        data,
        model_result,
        "walsby ETR I 20240925.csv",
        color_platt
      )
    )
  )
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
    expect_equal(model_result[["sdiff"]], 55.5823146)
    expect_equal(model_result[["a"]], 221.23782)
    expect_equal(model_result[["b"]], 0.387249932)
    expect_equal(model_result[["c"]], -0.035964253)
    expect_equal(model_result[["d"]], NA_real_)
    expect_equal(model_result[["alpha"]], 0.387249932)
    expect_equal(model_result[["beta"]], -0.035964253)
    expect_equal(model_result[["etrmax_with_photoinhibition"]], 151.8614464)
    expect_equal(model_result[["etrmax_without_photoinhibition"]], 221.23782)
    expect_equal(model_result[["ik_with_photoinhibition"]], 392.15358)
    expect_equal(model_result[["ik_without_photoinhibition"]], 571.3050)
    expect_equal(model_result[["im_with_photoinhibition"]], 1358.0)
    expect_equal(model_result[["w"]], NA_real_)
    expect_equal(model_result[["ib"]], NA_real_)
    expect_equal(model_result[["etrmax_with_without_ratio"]], 1.456839939)
  } else if (is_windows()) {
    expect_equal(model_result[["sdiff"]], 55.5823146)
    expect_equal(model_result[["a"]], 221.23782)
    expect_equal(model_result[["b"]], 0.387249932)
    expect_equal(model_result[["c"]], -0.035964253)
    expect_equal(model_result[["d"]], NA_real_)
    expect_equal(model_result[["alpha"]], 0.387249932)
    expect_equal(model_result[["beta"]], -0.035964253)
    expect_equal(model_result[["etrmax_with_photoinhibition"]], 151.8614464)
    expect_equal(model_result[["etrmax_without_photoinhibition"]], 221.23782)
    expect_equal(model_result[["ik_with_photoinhibition"]], 392.15358)
    expect_equal(model_result[["ik_without_photoinhibition"]], 571.3050)
    expect_equal(model_result[["im_with_photoinhibition"]], 1358.0)
    expect_equal(model_result[["w"]], NA_real_)
    expect_equal(model_result[["ib"]], NA_real_)
    expect_equal(model_result[["etrmax_with_without_ratio"]], 1.456839939)
  }
})

test_that("test-walsby_etr_I modified control plot 20240925.csv", {
  test_data_file <- file.path(getwd(), "data", "20240925.csv")
  data <- read_dual_pam_data(test_data_file)
  model_result <- walsby_generate_regression_ETR_I(data)
  model_result <- walsby_modified(model_result)

  expect_no_warning(
    print(
      plot_control(
        data,
        model_result,
        "walsby ETR I modified 20240925.csv",
        color_platt
      )
    )
  )
})
