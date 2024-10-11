test_that("test-platt_etr_I generate regression 20231122_01_W3_T20_HL.csv", {
  test_data_file <- file.path(getwd(), "data", "20231122_01_W3_T20_HL.csv")
  data <- read_pam_data(test_data_file)
  model_result <- platt_generate_regression_ETR_I(data)

  expect_equal(model_result[["sdiff"]], 1709.3953)
  expect_equal(model_result[["alpha"]], 0.23751254, tolerance = 0.0000001)
  expect_equal(model_result[["beta"]], 38.788443)
  expect_equal(model_result[["ps"]], 56442.6305)
  expect_equal(model_result[["pm"]], 126.756436)
  expect_equal(model_result[["ik"]], 533.68312)
  expect_equal(model_result[["is"]], 237640.63)
  expect_equal(model_result[["ib"]], 1455.1404)
  expect_equal(model_result[["im"]], 1450.7034)
})

test_that("test-platt_etr_I control plot 20231122_01_W3_T20_HL.csv", {
  test_data_file <- file.path(getwd(), "data", "20231122_01_W3_T20_HL.csv")
  data <- read_pam_data(test_data_file)
  model_result <- platt_generate_regression_ETR_I(data)

  expect_no_warning(
    print(
      platt_control_plot(
        data,
        model_result,
        "platt ETR I 20231122_01_W3_T20_HL.csv"
      )
    )
  )
})

test_that("test-platt_etr_I generate regression modified 20231122_01_W3_T20_HL.csv", {
  test_data_file <- file.path(getwd(), "data", "20231122_01_W3_T20_HL.csv")
  data <- read_pam_data(test_data_file)
  model_result <- platt_generate_regression_ETR_I(data)
  model_result <- platt_modified(model_result)

  expect_equal(model_result[["sdiff"]], 1709.3953)
  expect_equal(model_result[["a"]], 56442.6305)
  expect_equal(model_result[["b"]], 0.23751254, tolerance = 0.0000001)
  expect_equal(model_result[["c"]], 38.788443)
  expect_equal(model_result[["d"]], NA_real_)
  expect_equal(model_result[["alpha"]], 0.23751254, tolerance = 0.0000001)
  expect_equal(model_result[["beta"]], 38.788443)
  expect_equal(model_result[["etrmax_with_photoinhibition"]], 126.756436)
  expect_equal(model_result[["etrmax_without_photoinhibition"]], 56442.6305)
  expect_equal(model_result[["ik_with_photoinhibition"]], 533.68312)
  expect_equal(model_result[["ik_without_photoinhibition"]], 237640.63)
  expect_equal(model_result[["im_with_photoinhibition"]], 1450.7034)
  expect_equal(model_result[["w"]], NA_real_)
  expect_equal(model_result[["ib"]], 1455.1404)
  expect_equal(model_result[["etrmax_with_without_ratio"]], 56442.6305 / 126.756436)
})

test_that("test-platt_etr_I modified control plot 20231122_01_W3_T20_HL.csv", {
  test_data_file <- file.path(getwd(), "data", "20231122_01_W3_T20_HL.csv")
  data <- read_pam_data(test_data_file)
  model_result <- platt_generate_regression_ETR_I(data)
  model_result <- platt_modified(model_result)

  expect_no_warning(
    print(
      platt_control_plot(
        data,
        model_result,
        "platt ETR I modified 20231122_01_W3_T20_HL.csv"
      )
    )
  )
})
