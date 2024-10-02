test_that("test eilers_peeters ETR I 20231122_01_W3_T20_HL.csv", {
  test_data_file <- file.path(getwd(), "data", "20231122_01_W3_T20_HL.csv")
  data <- read_pam_data(test_data_file)
  model_result <- generate_regression_eilers_peeters_ETR_I(data)

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

test_that("test plot eilers_peeters ETR I 20231122_01_W3_T20_HL.csv", {
  test_data_file <- file.path(getwd(), "data", "20231122_01_W3_T20_HL.csv")
  data <- read_pam_data(test_data_file)
  model_result <- generate_regression_eilers_peeters_ETR_I(data)

  expect_no_warning(
    print(
      plot_control_eilers_peeters(
        data,
        model_result,
        etr_I_type,
        "eilers_peeters ETR I 20231122_01_W3_T20_HL.csv"
      )
    )
  )
})
