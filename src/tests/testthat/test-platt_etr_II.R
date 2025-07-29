test_that("test-platt_etr_II generate regression 20240925.csv", {
  test_data_file <- file.path(getwd(), "data", "20240925.csv")
  data <- read_dual_pam_data(test_data_file)
  model_result <- platt_generate_regression_ETR_II(data)

  if (is_supported_os() == FALSE) {
    skip("Unsupported operating system for this test.")
  }

  if (is_debian_or_ubuntu()) {
    expect_equal(model_result[["sdiff"]], 12.0348133)
    expect_equal(model_result[["alpha"]], 0.178182825)
    expect_equal(model_result[["beta"]], 0.035606479)
    expect_equal(model_result[["ps"]], 76.53219)
    expect_equal(model_result[["pm"]], 44.5824637)
    expect_equal(model_result[["ik"]], 250.2062903)
    expect_equal(model_result[["is"]], 429.514971)
    expect_equal(model_result[["ib"]], 2149.38944)
    expect_equal(model_result[["im"]], 769.88984)
  } else if (is_windows()) {
    expect_equal(model_result[["sdiff"]], 12.0348133)
    expect_equal(model_result[["alpha"]], 0.178182825)
    expect_equal(model_result[["beta"]], 0.035606479)
    expect_equal(model_result[["ps"]], 76.53219)
    expect_equal(model_result[["pm"]], 44.5824637)
    expect_equal(model_result[["ik"]], 250.2062903)
    expect_equal(model_result[["is"]], 429.514971)
    expect_equal(model_result[["ib"]], 2149.38944)
    expect_equal(model_result[["im"]], 769.88984)
  }
})

test_that("test-platt_etr_II control plot 20240925.csv", {
  test_data_file <- file.path(getwd(), "data", "20240925.csv")
  data <- read_dual_pam_data(test_data_file)
  model_result <- platt_generate_regression_ETR_II(data)

  expect_no_warning(
    print(
      plot_control(
        data,
        model_result,
        "platt ETR II 20240925.csv",
        color_platt
      )
    )
  )
})

test_that("test-platt_etr_II generate regression modified 20240925.csv", {
  test_data_file <- file.path(getwd(), "data", "20240925.csv")
  data <- read_dual_pam_data(test_data_file)
  model_result <- platt_generate_regression_ETR_II(data)
  model_result <- platt_modified(model_result)

  if (is_supported_os() == FALSE) {
    skip("Unsupported operating system for this test.")
  }

  if (is_debian_or_ubuntu()) {
    expect_equal(model_result[["sdiff"]], 12.0348133)
    expect_equal(model_result[["a"]], 76.53219)
    expect_equal(model_result[["b"]], 0.178182825)
    expect_equal(model_result[["c"]], 0.035606479)
    expect_equal(model_result[["d"]], NA_real_)
    expect_equal(model_result[["alpha"]], 0.178182825)
    expect_equal(model_result[["beta"]], 0.035606479)
    expect_equal(model_result[["etrmax_with_photoinhibition"]], 44.5824637)
    expect_equal(model_result[["etrmax_without_photoinhibition"]], 76.53219)
    expect_equal(model_result[["ik_with_photoinhibition"]], 250.2062903)
    expect_equal(model_result[["ik_without_photoinhibition"]], 429.514971)
    expect_equal(model_result[["im_with_photoinhibition"]], 769.88984)
    expect_equal(model_result[["w"]], NA_real_)
    expect_equal(model_result[["ib"]], 2149.38944)
    expect_equal(model_result[["etrmax_with_without_ratio"]], 1.71664337)
  } else if (is_windows()) {
    expect_equal(model_result[["sdiff"]], 12.0348133)
    expect_equal(model_result[["a"]], 76.53219)
    expect_equal(model_result[["b"]], 0.178182825)
    expect_equal(model_result[["c"]], 0.035606479)
    expect_equal(model_result[["d"]], NA_real_)
    expect_equal(model_result[["alpha"]], 0.178182825)
    expect_equal(model_result[["beta"]], 0.035606479)
    expect_equal(model_result[["etrmax_with_photoinhibition"]], 44.5824637)
    expect_equal(model_result[["etrmax_without_photoinhibition"]], 76.53219)
    expect_equal(model_result[["ik_with_photoinhibition"]], 250.2062903)
    expect_equal(model_result[["ik_without_photoinhibition"]], 429.514971)
    expect_equal(model_result[["im_with_photoinhibition"]], 769.88984)
    expect_equal(model_result[["w"]], NA_real_)
    expect_equal(model_result[["ib"]], 2149.38944)
    expect_equal(model_result[["etrmax_with_without_ratio"]], 1.71664337)
  }
})

test_that("test-platt_etr_II modified control plot 20240925.csv", {
  test_data_file <- file.path(getwd(), "data", "20240925.csv")
  data <- read_dual_pam_data(test_data_file)
  model_result <- platt_generate_regression_ETR_II(data)
  model_result <- platt_modified(model_result)

  expect_no_warning(
    print(
      plot_control(
        data,
        model_result,
        "platt ETR II modified 20240925.csv",
        color_platt
      )
    )
  )
})
