test_that("test-eilers_peeters_etr_II generate regression 20240925.csv", {
  test_data_file <- file.path(getwd(), "data", "20240925.csv")
  data <- read_dual_pam_data(test_data_file)
  model_result <- eilers_peeters_generate_regression_ETR_II(data)

  os_name <- Sys.info()[["sysname"]]

  if (os_name == "Linux") {
    expect_equal(model_result[["sdiff"]], 5.7818334)
    #expect_equal(model_result[["a"]], 0)
    #expect_equal(model_result[["b"]], 0.005765059)
    expect_equal(model_result[["c"]], 6.1088971)
    expect_equal(model_result[["pm"]], 44.5224748)
    expect_equal(model_result[["s"]], 0.163695670)
    expect_equal(model_result[["ik"]], 271.98322)
    expect_equal(model_result[["im"]], 731.801329)
    expect_equal(model_result[["w"]], 0.690612084)
  } else if (os_name == "Windows") {
    expect_equal(model_result[["sdiff"]], 5.7818334)
    #expect_equal(model_result[["a"]], 0)
    #expect_equal(model_result[["b"]], 0.005765059)
    expect_equal(model_result[["c"]], 6.1088971)
    expect_equal(model_result[["pm"]], 44.5224748)
    expect_equal(model_result[["s"]], 0.163695670)
    expect_equal(model_result[["ik"]], 271.98322)
    expect_equal(model_result[["im"]], 731.801329)
    expect_equal(model_result[["w"]], 0.690612084)
  } else {
    stop(paste("Unsupported operating system:", os_name))
  }
})


test_that("test-eilers_peeters_etr_II modified 20240925.csv", {
  test_data_file <- file.path(getwd(), "data", "20240925.csv")
  data <- read_dual_pam_data(test_data_file)
  model_result <- eilers_peeters_generate_regression_ETR_II(data)

  expect_no_warning(
    model_result <- eilers_peeters_modified(model_result)
  )

  os_name <- Sys.info()[["sysname"]]

  if (os_name == "Linux") {
    expect_equal(model_result[["sdiff"]], 5.7818334)
    #expect_equal(model_result[["a"]], 0)
    #expect_equal(model_result[["b"]], 0.005765059)
    expect_equal(model_result[["c"]], 6.1088971)
    expect_equal(model_result[["d"]], NA_real_)
    expect_equal(model_result[["alpha"]], 0.163695670)
    expect_equal(model_result[["beta"]], NA_real_)
    expect_equal(model_result[["etrmax_with_photoinhibition"]], 44.5224748)
    expect_equal(model_result[["etrmax_without_photoinhibition"]], NA_real_)
    expect_equal(model_result[["ik_with_photoinhibition"]], 271.98322)
    expect_equal(model_result[["ik_without_photoinhibition"]], NA_real_)
    expect_equal(model_result[["im_with_photoinhibition"]], 731.801329)
    expect_equal(model_result[["w"]], 0.690612084)
    expect_equal(model_result[["ib"]], NA_real_)
    expect_equal(model_result[["etrmax_with_without_ratio"]], NA_real_)
  } else if (os_name == "Windows") {
    expect_equal(model_result[["sdiff"]], 5.7818334)
    #expect_equal(model_result[["a"]], 0)
    #expect_equal(model_result[["b"]], 0.005765059)
    expect_equal(model_result[["c"]], 6.1088971)
    expect_equal(model_result[["d"]], NA_real_)
    expect_equal(model_result[["alpha"]], 0.163695670)
    expect_equal(model_result[["beta"]], NA_real_)
    expect_equal(model_result[["etrmax_with_photoinhibition"]], 44.5224748)
    expect_equal(model_result[["etrmax_without_photoinhibition"]], NA_real_)
    expect_equal(model_result[["ik_with_photoinhibition"]], 271.98322)
    expect_equal(model_result[["ik_without_photoinhibition"]], NA_real_)
    expect_equal(model_result[["im_with_photoinhibition"]], 731.801329)
    expect_equal(model_result[["w"]], 0.690612084)
    expect_equal(model_result[["ib"]], NA_real_)
    expect_equal(model_result[["etrmax_with_without_ratio"]], NA_real_)
  } else {
    stop(paste("Unsupported operating system:", os_name))
  }
})


test_that("test-eilers_peeters_etr_II modified control plot 20240925.csv", {
  test_data_file <- file.path(getwd(), "data", "20240925.csv")
  data <- read_dual_pam_data(test_data_file)
  model_result <- eilers_peeters_generate_regression_ETR_II(data)
  model_result <- model_result <- eilers_peeters_modified(model_result)

  expect_no_warning(
    print(
      plot <- plot_control(
        data,
        model_result,
        "eilers_peeters ETR II modified 20240925.csv",
        color_eilers_peeters
      )
    )
  )
})
