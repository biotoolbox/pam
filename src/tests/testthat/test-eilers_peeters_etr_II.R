test_that("test-eilers_peeters_etr_II generate regression 20240925.csv", {
  test_data_file <- file.path(getwd(), "data", "20240925.csv")
  data <- read_dual_pam_data(test_data_file)
  model_result <- eilers_peeters_generate_regression_ETR_II(data)

  if (is_supported_os() == FALSE) {
    skip("Unsupported operating system for this test.")
  }

  if (is_debian_or_ubuntu()) {
    expect_equal(model_result[["residual_sum_of_squares"]], 5.7818334)
    # expect_equal(model_result[["a"]], 0)
    # expect_equal(model_result[["b"]], 0.005765059)
    expect_equal(model_result[["c"]], 6.10889648)
    expect_equal(model_result[["pm"]], 44.52247403)
    expect_equal(model_result[["s"]], 0.163695686)
    expect_equal(model_result[["ik"]], 271.983185)
    expect_equal(model_result[["im"]], 731.801311)
    expect_equal(model_result[["w"]], 0.69061233)
  } else if (is_windows()) {
    expect_equal(model_result[["residual_sum_of_squares"]], 5.7818334)
    # expect_equal(model_result[["a"]], 0)
    # expect_equal(model_result[["b"]], 0.005765059)
    expect_equal(model_result[["c"]], 6.10889648)
    expect_equal(model_result[["pm"]], 44.52247403)
    expect_equal(model_result[["s"]], 0.163695686)
    expect_equal(model_result[["ik"]], 271.983185)
    expect_equal(model_result[["im"]], 731.801311)
    expect_equal(model_result[["w"]], 0.69061233)
  }
})

test_that("test-eilers_peeters_etr_II modified 20240925.csv", {
  test_data_file <- file.path(getwd(), "data", "20240925.csv")
  data <- read_dual_pam_data(test_data_file)
  model_result <- eilers_peeters_generate_regression_ETR_II(data)

  expect_no_warning(
    model_result <- eilers_peeters_modified(model_result)
  )

  if (is_supported_os() == FALSE) {
    skip("Unsupported operating system for this test.")
  }

  if (is_debian_or_ubuntu()) {
    expect_equal(model_result[["residual_sum_of_squares"]], 5.7818334)
    # expect_equal(model_result[["a"]], 0)
    # expect_equal(model_result[["b"]], 0.005765059)
    expect_equal(model_result[["c"]], 6.108896488)
    expect_equal(model_result[["d"]], NA_real_)
    expect_equal(model_result[["alpha"]], 0.163695686)
    expect_equal(model_result[["beta"]], NA_real_)
    expect_equal(model_result[["etrmax_with_photoinhibition"]], 44.52247403)
    expect_equal(model_result[["etrmax_without_photoinhibition"]], NA_real_)
    expect_equal(model_result[["ik_with_photoinhibition"]], 271.983185)
    expect_equal(model_result[["ik_without_photoinhibition"]], NA_real_)
    expect_equal(model_result[["im_with_photoinhibition"]], 731.801311)
    expect_equal(model_result[["w"]], 0.69061233)
    expect_equal(model_result[["ib"]], NA_real_)
    expect_equal(model_result[["etrmax_with_without_ratio"]], NA_real_)
  } else if (is_windows()) {
    expect_equal(model_result[["residual_sum_of_squares"]], 5.7818334)
    # expect_equal(model_result[["a"]], 0)
    # expect_equal(model_result[["b"]], 0.005765059)
    expect_equal(model_result[["c"]], 6.10889648)
    expect_equal(model_result[["d"]], NA_real_)
    expect_equal(model_result[["alpha"]], 0.163695686)
    expect_equal(model_result[["beta"]], NA_real_)
    expect_equal(model_result[["etrmax_with_photoinhibition"]], 44.52247403)
    expect_equal(model_result[["etrmax_without_photoinhibition"]], NA_real_)
    expect_equal(model_result[["ik_with_photoinhibition"]], 271.983185)
    expect_equal(model_result[["ik_without_photoinhibition"]], NA_real_)
    expect_equal(model_result[["im_with_photoinhibition"]], 731.801311)
    expect_equal(model_result[["w"]], 0.69061233)
    expect_equal(model_result[["ib"]], NA_real_)
    expect_equal(model_result[["etrmax_with_without_ratio"]], NA_real_)
  }
})

test_that("test-eilers_peeters_etr_II modified control plot 20240925.csv", {
  test_data_file <- file.path(getwd(), "data", "20240925.csv")
  data <- read_dual_pam_data(test_data_file)
  model_result <- eilers_peeters_generate_regression_ETR_II(data)
  model_result <- model_result <- eilers_peeters_modified(model_result)

  expect_no_warning({
    plot <- plot_control(
      data,
      model_result,
      "eilers_peeters ETR II modified 20240925.csv",
      color_eilers_peeters
    )
    ggplot2::ggsave("results/test-eilers_peeters_etr_II modified control plot 20240925.jpg", create.dir = TRUE, plot = plot, units = "px", width = 1000, height = 1000, dpi = 100, limitsize = FALSE)
  })
})
