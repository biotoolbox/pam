test_that("test-eilers_peeters_etr_I generate regression 20231122_01_W3_T20_HL.csv", {
  test_data_file <- file.path(getwd(), "data", "20231122_01_W3_T20_HL.csv")
  data <- read_pam_data(test_data_file)
  model_result <- eilers_peeters_generate_regression_ETR_I(data)

  expect_equal(model_result[["sdiff"]], 1183.4214)
  # expect_equal(model_result[["a"]], 0.0000062986, tolerance = 0.0001)
  expect_equal(model_result[["b"]], -0.00523754, tolerance = 0.00001)
  expect_equal(model_result[["c"]], 6.8374816)
  expect_equal(model_result[["pm"]], 126.78253)
  expect_equal(model_result[["s"]], 0.14625268)
  expect_equal(model_result[["ik"]], 866.87322)
  expect_equal(model_result[["im"]], 1041.89685)
  expect_equal(model_result[["w"]], -0.79809778)
})

test_that("test-eilers_peeters_etr_I control plot 20231122_01_W3_T20_HL.csv", {
  test_data_file <- file.path(getwd(), "data", "20231122_01_W3_T20_HL.csv")
  data <- read_pam_data(test_data_file)
  model_result <- eilers_peeters_generate_regression_ETR_I(data)

  expect_no_warning(
    print(
      plot_control(
        data,
        model_result,
        "eilers_peeters ETR I 20231122_01_W3_T20_HL.csv",
        color_eilers_peeters
      )
    )
  )
})

test_that("test-eilers_peeters_etr_I generate regression modified 20231122_01_W3_T20_HL.csv", {
  test_data_file <- file.path(getwd(), "data", "20231122_01_W3_T20_HL.csv")
  data <- read_pam_data(test_data_file)
  model_result <- eilers_peeters_generate_regression_ETR_I(data)
  expect_no_warning(
    model_result <- eilers_peeters_modified(model_result)
  )

  expect_equal(model_result[["sdiff"]], 1183.4214)
  # expect_equal(model_result[["a"]], 0.0000062986, tolerance = 0.0001)
  expect_equal(model_result[["b"]], -0.00523754, tolerance = 0.00001)
  expect_equal(model_result[["c"]], 6.8374816)
  expect_equal(model_result[["d"]], NA_real_)
  expect_equal(model_result[["alpha"]], 0.14625268)
  expect_equal(model_result[["beta"]], NA_real_)
  expect_equal(model_result[["etrmax_with_photoinhibition"]], 126.78253)
  expect_equal(model_result[["etrmax_without_photoinhibition"]], NA_real_)
  expect_equal(model_result[["ik_with_photoinhibition"]], 866.87322)
  expect_equal(model_result[["ik_without_photoinhibition"]], NA_real_)
  expect_equal(model_result[["im_with_photoinhibition"]], 1041.89685)
  expect_equal(model_result[["w"]], -0.79809778)
  expect_equal(model_result[["ib"]], NA_real_)
  expect_equal(model_result[["etrmax_with_without_ratio"]], NA_real_)
})

test_that("test-eilers_peeters_etr_I modified control plot 20231122_01_W3_T20_HL.csv", {
  test_data_file <- file.path(getwd(), "data", "20231122_01_W3_T20_HL.csv")
  data <- read_pam_data(test_data_file)
  model_result <- eilers_peeters_generate_regression_ETR_I(data)
  model_result <- eilers_peeters_modified(model_result)

  expect_no_warning(
    print(
      plot_control(
        data,
        model_result,
        "eilers_peeters ETR I modified 20231122_01_W3_T20_HL.csv",
        color_eilers_peeters
      )
    )
  )
})
