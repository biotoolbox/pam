test_that("test-walsby_etr_II generate regression 20231122_01_W3_T20_HL.csv", {
  test_data_file <- file.path(getwd(), "data", "20231122_01_W3_T20_HL.csv")
  data <- read_pam_data(test_data_file)
  model_result <- walsby_generate_regression_ETR_II(data)

  expect_equal(model_result[["sdiff"]], 4.4596755)
  expect_equal(model_result[["etr_max"]], 47.999349)
  expect_equal(model_result[["alpha"]], 0.11605834)
  expect_equal(model_result[["beta"]], -0.012745, tolerance = 0.000001)
})

test_that("test-walsby_etr_II control plot 20231122_01_W3_T20_HL.csv", {
  test_data_file <- file.path(getwd(), "data", "20231122_01_W3_T20_HL.csv")
  data <- read_pam_data(test_data_file)
  model_result <- walsby_generate_regression_ETR_II(data)

  expect_no_warning(
    print(
      walsby_control_plot(
        data,
        model_result,
        "walsby ETR II 20231122_01_W3_T20_HL.csv"
      )
    )
  )
})

test_that("test-walsby_etr_II generate regression modified 20231122_01_W3_T20_HL.csv", {
  test_data_file <- file.path(getwd(), "data", "20231122_01_W3_T20_HL.csv")
  data <- read_pam_data(test_data_file)
  model_result <- walsby_generate_regression_ETR_II(data)
  model_result <- walsby_modified(model_result)

  expect_equal(model_result[["sdiff"]], 4.4596755)
  expect_equal(model_result[["a"]], 47.999349)
  expect_equal(model_result[["b"]], 0.11605834)
  expect_equal(model_result[["c"]], -0.012745, tolerance = 0.000001)
  expect_equal(model_result[["d"]], NA_real_)
  expect_equal(model_result[["alpha"]], 0.11605834)
  expect_equal(model_result[["beta"]], -0.012745, tolerance = 0.000001)
  expect_equal(model_result[["etrmax_with_photoinhibition"]], 31.0847209)
  expect_equal(model_result[["etrmax_without_photoinhibition"]], 47.999349)
  expect_equal(model_result[["ik_with_photoinhibition"]], 267.83703)
  expect_equal(model_result[["ik_without_photoinhibition"]], 413.57949)
  expect_equal(model_result[["im_with_photoinhibition"]], 914)
  expect_equal(model_result[["w"]], NA_real_)
  expect_equal(model_result[["ib"]], NA_real_)
  expect_equal(model_result[["etrmax_with_without_ratio"]], 1.54414603)
})

test_that("test-walsby_etr_II modified control plot 20231122_01_W3_T20_HL.csv", {
  test_data_file <- file.path(getwd(), "data", "20231122_01_W3_T20_HL.csv")
  data <- read_pam_data(test_data_file)
  model_result <- walsby_generate_regression_ETR_II(data)
  model_result <- walsby_modified(model_result)

  expect_no_warning(
    print(
      walsby_control_plot(
        data,
        model_result,
        "walsby ETR II modified 20231122_01_W3_T20_HL.csv"
      )
    )
  )
})
