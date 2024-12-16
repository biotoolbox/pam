test_that("test-platt_etr_II generate regression 20231122_01.csv", {
  test_data_file <- file.path(getwd(), "data", "20231122_01.csv")
  data <- read_dual_pam_data(test_data_file)
  model_result <- platt_generate_regression_ETR_II(data)

  expect_equal(model_result[["sdiff"]], 4.4844798)
  expect_equal(model_result[["alpha"]], 0.1028022)
  expect_equal(model_result[["beta"]], 0.02631246, tolerance = 0.000001)
  expect_equal(model_result[["ps"]], 58.592426)
  expect_equal(model_result[["pm"]], 31.049374)
  expect_equal(model_result[["ik"]], 302.03024)
  expect_equal(model_result[["is"]], 569.95303)
  expect_equal(model_result[["ib"]], 2226.79356)
  expect_equal(model_result[["im"]], 906.60042)
})

test_that("test-platt_etr_II control plot 20231122_01.csv", {
  test_data_file <- file.path(getwd(), "data", "20231122_01.csv")
  data <- read_dual_pam_data(test_data_file)
  model_result <- platt_generate_regression_ETR_II(data)

  expect_no_warning(
    print(
      plot_control(
        data,
        model_result,
        "platt ETR II 20231122_01.csv",
        color_platt
      )
    )
  )
})

test_that("test-platt_etr_II generate regression modified 20231122_01.csv", {
  test_data_file <- file.path(getwd(), "data", "20231122_01.csv")
  data <- read_dual_pam_data(test_data_file)
  model_result <- platt_generate_regression_ETR_II(data)
  model_result <- platt_modified(model_result)

  expect_equal(model_result[["sdiff"]], 4.4844798)
  expect_equal(model_result[["a"]], 58.592426)
  expect_equal(model_result[["b"]], 0.1028022)
  expect_equal(model_result[["c"]], 0.02631246, tolerance = 0.000001)
  expect_equal(model_result[["d"]], NA_real_)
  expect_equal(model_result[["alpha"]], 0.1028022)
  expect_equal(model_result[["beta"]], 0.02631246, tolerance = 0.000001)
  expect_equal(model_result[["etrmax_with_photoinhibition"]], 31.049374)
  expect_equal(model_result[["etrmax_without_photoinhibition"]], 58.592426)
  expect_equal(model_result[["ik_with_photoinhibition"]], 302.03024)
  expect_equal(model_result[["ik_without_photoinhibition"]], 569.95303)
  expect_equal(model_result[["im_with_photoinhibition"]], 906.60042)
  expect_equal(model_result[["w"]], NA_real_)
  expect_equal(model_result[["ib"]], 2226.79356)
  expect_equal(model_result[["etrmax_with_without_ratio"]], 58.592426 / 31.049374)
})

test_that("test-platt_etr_II modified control plot 20231122_01.csv", {
  test_data_file <- file.path(getwd(), "data", "20231122_01.csv")
  data <- read_dual_pam_data(test_data_file)
  model_result <- platt_generate_regression_ETR_II(data)
  model_result <- platt_modified(model_result)

  expect_no_warning(
    print(
      plot_control(
        data,
        model_result,
        "platt ETR II modified 20231122_01.csv",
        color_platt
      )
    )
  )
})
