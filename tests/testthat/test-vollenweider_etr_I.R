test_that("test-vollenweider_etr_I generate regression 20231122_01_W3_T20_HL.csv", {
  test_data_file <- file.path(getwd(), "data", "20231122_01_W3_T20_HL.csv")
  data <- read_pam_data(test_data_file)
  model_result <- vollenweider_generate_regression_ETR_I(data)

  expect_equal(model_result[["sdiff"]], 1099.94915)
  expect_equal(model_result[["pmax"]], 1132.09578)
  expect_equal(model_result[["a"]], 0.00016833, tolerance = 0.00001)
  expect_equal(model_result[["alpha"]], -0.00001382, tolerance = 0.0001)
  expect_equal(model_result[["n"]], 4224.5959)
  expect_equal(model_result[["ik"]], 5940.6891)
  expect_equal(model_result[["popt"]], 126.51602)
  expect_equal(model_result[["iik"]], 663.89465)
  expect_equal(model_result[["pmax_popt_and_ik_iik_ratio"]], 8.9482406)
})

test_that("test-vollenweider_etr_I control plot 20231122_01_W3_T20_HL.csv", {
  test_data_file <- file.path(getwd(), "data", "20231122_01_W3_T20_HL.csv")
  data <- read_pam_data(test_data_file)
  model_result <- vollenweider_generate_regression_ETR_I(data)

  expect_no_warning(
    print(
      vollenweider_control_plot(
        data,
        model_result,
        "vollenweider ETR I 20231122_01_W3_T20_HL.csv"
      )
    )
  )
})

test_that("test-vollenweider_etr_I generate regression modified 20231122_01_W3_T20_HL.csv", {
  test_data_file <- file.path(getwd(), "data", "20231122_01_W3_T20_HL.csv")
  data <- read_pam_data(test_data_file)
  model_result <- vollenweider_generate_regression_ETR_I(data)
  model_result <- vollenweider_modified(model_result)

  expect_equal(model_result[["sdiff"]], 1099.94915)
  expect_equal(model_result[["a"]], 1132.09578)
  expect_equal(model_result[["b"]], 0.00016833, tolerance = 0.00001)
  expect_equal(model_result[["c"]], -0.00001382, tolerance = 0.0001)
  expect_equal(model_result[["d"]], 4224.5959)
  expect_equal(model_result[["alpha"]], 126.51602 / 663.89465)
  expect_equal(model_result[["beta"]], NA_real_)
  expect_equal(model_result[["etrmax_with_photoinhibition"]], 126.51602)
  expect_equal(model_result[["etrmax_without_photoinhibition"]], 1132.09578)
  expect_equal(model_result[["ik_with_photoinhibition"]], 663.89465)
  expect_equal(model_result[["ik_without_photoinhibition"]], 5940.6891)
  expect_equal(model_result[["im_with_photoinhibition"]], 1095)
  expect_equal(model_result[["w"]], NA_real_)
  expect_equal(model_result[["ib"]], NA_real_)
  expect_equal(model_result[["etrmax_with_without_ratio"]], 8.9482406)
})

test_that("test-vollenweider_etr_I modified control plot 20231122_01_W3_T20_HL.csv", {
  test_data_file <- file.path(getwd(), "data", "20231122_01_W3_T20_HL.csv")
  data <- read_pam_data(test_data_file)
  model_result <- vollenweider_generate_regression_ETR_I(data)
  model_result <- vollenweider_modified(model_result)

  expect_no_warning(
    print(
      vollenweider_control_plot(
        data,
        model_result,
        "vollenweider ETR I modified 20231122_01_W3_T20_HL.csv"
      )
    )
  )
})
