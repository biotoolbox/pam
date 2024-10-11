test_that("test-walsby_etr_I generate regression 20231122_01_W3_T20_HL.csv", {
  test_data_file <- file.path(getwd(), "data", "20231122_01_W3_T20_HL.csv")
  data <- read_pam_data(test_data_file)
  model_result <- walsby_generate_regression_ETR_I(data)

  expect_equal(model_result[["sdiff"]], 1247.65184)
  expect_equal(model_result[["etr_max"]], 3970930.3)
  expect_equal(model_result[["alpha"]], 27.690236)
  expect_equal(model_result[["beta"]], -27.4705392)
})

test_that("test-walsby_etr_I control plot 20231122_01_W3_T20_HL.csv", {
  test_data_file <- file.path(getwd(), "data", "20231122_01_W3_T20_HL.csv")
  data <- read_pam_data(test_data_file)
  model_result <- walsby_generate_regression_ETR_I(data)

  expect_no_warning(
    print(
      walsby_control_plot(
        data,
        model_result,
        "walsby ETR I 20231122_01_W3_T20_HL.csv"
      )
    )
  )
})

test_that("test-walsby_etr_I generate regression modified 20231122_01_W3_T20_HL.csv", {
  test_data_file <- file.path(getwd(), "data", "20231122_01_W3_T20_HL.csv")
  data <- read_pam_data(test_data_file)
  model_result <- walsby_generate_regression_ETR_I(data)
  model_result <- walsby_modified(model_result)

  expect_equal(model_result[["sdiff"]], 1247.65184)
  expect_equal(model_result[["a"]], 3970930.3)
  expect_equal(model_result[["b"]], 27.690236)
  expect_equal(model_result[["c"]], -27.4705392)
  expect_equal(model_result[["d"]], NA_real_)
  expect_equal(model_result[["alpha"]], 27.690236)
  expect_equal(model_result[["beta"]], -27.4705392)
  expect_equal(model_result[["etrmax_with_photoinhibition"]], 125.31646)
  expect_equal(model_result[["etrmax_without_photoinhibition"]], 3970930.3)
  expect_equal(model_result[["ik_with_photoinhibition"]], 4.5256552)
  expect_equal(model_result[["ik_without_photoinhibition"]], 143405.433)
  expect_equal(model_result[["im_with_photoinhibition"]], 1142)
  expect_equal(model_result[["w"]], NA_real_)
  expect_equal(model_result[["ib"]], NA_real_)
  expect_equal(model_result[["etrmax_with_without_ratio"]], 31687.22)
})

test_that("test-walsby_etr_I modified control plot 20231122_01_W3_T20_HL.csv", {
  test_data_file <- file.path(getwd(), "data", "20231122_01_W3_T20_HL.csv")
  data <- read_pam_data(test_data_file)
  model_result <- walsby_generate_regression_ETR_I(data)
  model_result <- walsby_modified(model_result)

  expect_no_warning(
    print(
      walsby_control_plot(
        data,
        model_result,
        "walsby ETR I modified 20231122_01_W3_T20_HL.csv"
      )
    )
  )
})
