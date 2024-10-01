test_that("test eilers_peeters ETR II 20231122_01_W3_T20_HL.csv", {
  test_data_file <- file.path(getwd(), "data", "20231122_01_W3_T20_HL.csv")
  data <- read_pam_data(test_data_file)
  reg_data <- generate_regression_eilers_peeters_ETR_II(data)

  expect_equal(reg_data[["sdiff"]], 4.0658931)
  expect_equal(reg_data[["a"]], 0.00001294, tolerance = 0.0001)
  expect_equal(reg_data[["b"]], 0.009423141, tolerance = 0.0000001)
  expect_equal(reg_data[["c"]], 10.204545)
  expect_equal(reg_data[["pm"]], 30.862193)
  expect_equal(reg_data[["s"]], 0.09799555)
  expect_equal(reg_data[["ik"]], 314.93464)
  expect_equal(reg_data[["im"]], 888.16408)
  expect_equal(reg_data[["w"]], 0.8201537)
})

test_that("test plot eilers_peeters ETR II 20231122_01_W3_T20_HL.csv", {
  test_data_file <- file.path(getwd(), "data", "20231122_01_W3_T20_HL.csv")
  data <- read_pam_data(test_data_file)
  reg_data <- generate_regression_eilers_peeters_ETR_II(data)

  expect_no_warning(
    print(
      plot_control_eilers_peeters(
        data,
        reg_data,
        "eilers_peeters ETR II 20231122_01_W3_T20_HL.csv",
        etr_II_type
      )
    )
  )
})
