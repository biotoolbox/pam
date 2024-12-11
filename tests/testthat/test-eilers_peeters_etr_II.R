test_that("test-eilers_peeters_etr_II generate regression 20231122_01.csv", {
  test_data_file <- file.path(getwd(), "data", "20231122_01.csv")
  data <- read_dual_pam_data(test_data_file)
  model_result <- eilers_peeters_generate_regression_ETR_II(data)

  expect_equal(model_result[["sdiff"]], 4.0658931)
  expect_equal(model_result[["a"]], 0.00001294, tolerance = 0.0001)
  expect_equal(model_result[["b"]], 0.009423141, tolerance = 0.0000001)
  expect_equal(model_result[["c"]], 10.204545)
  expect_equal(model_result[["pm"]], 30.862193)
  expect_equal(model_result[["s"]], 0.09799555)
  expect_equal(model_result[["ik"]], 314.93464)
  expect_equal(model_result[["im"]], 888.16408)
  expect_equal(model_result[["w"]], 0.8201537)
})

test_that("test-eilers_peeters_etr_II control plot 20231122_01.csv", {
  test_data_file <- file.path(getwd(), "data", "20231122_01.csv")
  data <- read_dual_pam_data(test_data_file)
  model_result <- eilers_peeters_generate_regression_ETR_II(data)

  expect_no_warning(
    print(
      plot_control(
        data,
        model_result,
        "eilers_peeters ETR II 20231122_01.csv",
        color_eilers_peeters
      )
    )
  )
})

test_that("test-eilers_peeters_etr_II modified 20231122_01.csv", {
  test_data_file <- file.path(getwd(), "data", "20231122_01.csv")
  data <- read_dual_pam_data(test_data_file)
  model_result <- eilers_peeters_generate_regression_ETR_II(data)
  expect_no_warning(
    model_result <- eilers_peeters_modified(model_result)
  )

  expect_equal(model_result[["sdiff"]], 4.0658931)
  expect_equal(model_result[["a"]], 0.00001294, tolerance = 0.0001)
  expect_equal(model_result[["b"]], 0.009423141, tolerance = 0.0000001)
  expect_equal(model_result[["c"]], 10.204545)
  expect_equal(model_result[["d"]], NA_real_)
  expect_equal(model_result[["alpha"]], 0.09799555)
  expect_equal(model_result[["beta"]], NA_real_)
  expect_equal(model_result[["etrmax_with_photoinhibition"]], 30.862193)
  expect_equal(model_result[["etrmax_without_photoinhibition"]], NA_real_)
  expect_equal(model_result[["ik_with_photoinhibition"]], 314.93464)
  expect_equal(model_result[["ik_without_photoinhibition"]], NA_real_)
  expect_equal(model_result[["im_with_photoinhibition"]], 888.16408)
  expect_equal(model_result[["w"]], 0.8201537)
  expect_equal(model_result[["ib"]], NA_real_)
  expect_equal(model_result[["etrmax_with_without_ratio"]], NA_real_)
})

test_that("test-eilers_peeters_etr_II modified control plot 20231122_01.csv", {
  test_data_file <- file.path(getwd(), "data", "20231122_01.csv")
  data <- read_dual_pam_data(test_data_file)
  model_result <- eilers_peeters_generate_regression_ETR_II(data)
  model_result <- model_result <- eilers_peeters_modified(model_result)

  expect_no_warning(
    print(
      plot <- plot_control(
        data,
        model_result,
        "eilers_peeters ETR II modified 20231122_01.csv",
        color_eilers_peeters
      )
    )
  )
  ggplot2::ggsave("eilers_peeters ETR II modified.jpg", plot = plot, units = "px", width = 1000, height = 1200, dpi = 100, limitsize = FALSE)
})
