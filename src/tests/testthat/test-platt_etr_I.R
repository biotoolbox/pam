test_that("test-platt_etr_I generate regression 20240925.csv", {
  test_data_file <- file.path(getwd(), "data", "20240925.csv")
  data <- read_dual_pam_data(test_data_file)
  model_result <- platt_generate_regression_ETR_I(data)

  if (is_supported_os() == FALSE) {
    skip("Unsupported operating system for this test.")
  }

  if (is_debian_or_ubuntu()) {
    expect_equal(model_result[["sdiff"]], 55.4812913)
    expect_equal(model_result[["alpha"]], 0.350792430)
    expect_equal(model_result[["beta"]], 0.056258810)
    expect_equal(model_result[["ps"]], 242.02871)
    expect_equal(model_result[["pm"]], 151.85573)
    expect_equal(model_result[["ik"]], 432.8934096)
    expect_equal(model_result[["is"]], 689.94849)
    expect_equal(model_result[["ib"]], 4302.05878)
    expect_equal(model_result[["im"]], 1365.3918)
  } else if (is_windows()) {
    expect_equal(model_result[["sdiff"]], 55.4812913)
    expect_equal(model_result[["alpha"]], 0.350792430)
    expect_equal(model_result[["beta"]], 0.056258810)
    expect_equal(model_result[["ps"]], 242.02871)
    expect_equal(model_result[["pm"]], 151.85573)
    expect_equal(model_result[["ik"]], 432.8934096)
    expect_equal(model_result[["is"]], 689.94849)
    expect_equal(model_result[["ib"]], 4302.05878)
    expect_equal(model_result[["im"]], 1365.3918)
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
    expect_equal(model_result[["sdiff"]], 55.4812913)
    expect_equal(model_result[["a"]], 242.02871)
    expect_equal(model_result[["b"]], 0.350792430)
    expect_equal(model_result[["c"]], 0.056258810)
    expect_equal(model_result[["d"]], NA_real_)
    expect_equal(model_result[["alpha"]], 0.350792430)
    expect_equal(model_result[["beta"]], 0.056258810)
    expect_equal(model_result[["etrmax_with_photoinhibition"]], 151.85573)
    expect_equal(model_result[["etrmax_without_photoinhibition"]], 242.02871)
    expect_equal(model_result[["ik_with_photoinhibition"]], 432.8934096)
    expect_equal(model_result[["ik_without_photoinhibition"]], 689.94849)
    expect_equal(model_result[["im_with_photoinhibition"]], 1365.3918)
    expect_equal(model_result[["w"]], NA_real_)
    expect_equal(model_result[["ib"]], 4302.05878)
    expect_equal(model_result[["etrmax_with_without_ratio"]], 1.593806867)
  } else if (is_windows()) {
    expect_equal(model_result[["sdiff"]], 55.4812913)
    expect_equal(model_result[["a"]], 242.02871)
    expect_equal(model_result[["b"]], 0.350792430)
    expect_equal(model_result[["c"]], 0.056258810)
    expect_equal(model_result[["d"]], NA_real_)
    expect_equal(model_result[["alpha"]], 0.350792430)
    expect_equal(model_result[["beta"]], 0.056258810)
    expect_equal(model_result[["etrmax_with_photoinhibition"]], 151.85573)
    expect_equal(model_result[["etrmax_without_photoinhibition"]], 242.02871)
    expect_equal(model_result[["ik_with_photoinhibition"]], 432.8934096)
    expect_equal(model_result[["ik_without_photoinhibition"]], 689.94849)
    expect_equal(model_result[["im_with_photoinhibition"]], 1365.3918)
    expect_equal(model_result[["w"]], NA_real_)
    expect_equal(model_result[["ib"]], 4302.05878)
    expect_equal(model_result[["etrmax_with_without_ratio"]], 1.593806867)
  }
})

test_that("test-platt_etr_I modified control plot 20240925.csv", {
  test_data_file <- file.path(getwd(), "data", "20240925.csv")
  data <- read_dual_pam_data(test_data_file)
  model_result <- platt_generate_regression_ETR_I(data)
  model_result <- platt_modified(model_result)

  expect_no_warning(
    print(
      plot_control(
        data,
        model_result,
        "platt ETR I modified 20240925.csv",
        color_platt
      )
    )
  )
})
