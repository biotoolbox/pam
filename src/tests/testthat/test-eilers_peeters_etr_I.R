test_that("test-eilers_peeters_etr_I generate regression 20240925.csv", {
  test_data_file <- file.path(getwd(), "data", "20240925.csv")
  data <- read_dual_pam_data(test_data_file)
  model_result <- eilers_peeters_generate_regression_ETR_I(data)

  os_name <- Sys.info()[["sysname"]]

  if (os_name == "Linux") {
    os_release <- readLines("/etc/os-release")
    distro_line <- grep("^ID=", os_release, value = TRUE)
    distro <- sub("^ID=", "", distro_line)

    if (distro == "debian" || distro == "ubuntu") {
      expect_equal(model_result[["sdiff"]], 53.4454062)
      #expect_equal(model_result[["a"]], 0.000001479)
      #expect_equal(model_result[["b"]], 0.002444096)
      expect_equal(model_result[["c"]], 2.8984079)
      expect_equal(model_result[["pm"]], 151.851986)
      expect_equal(model_result[["s"]], 0.345017)
      expect_equal(model_result[["ik"]], 440.1289906)
      expect_equal(model_result[["im"]], 1399.76963)
      expect_equal(model_result[["w"]], 1.180362267)
    } else {
      skip(paste("Skipping test on unsupported Linux distribution:", distro))
    }
  } else if (os_name == "Windows") {
    expect_equal(model_result[["sdiff"]], 53.4454062)
    #expect_equal(model_result[["a"]], 0.000001479)
    #expect_equal(model_result[["b"]], 0.002444096)
    expect_equal(model_result[["c"]], 2.8984079)
    expect_equal(model_result[["pm"]], 151.851986)
    expect_equal(model_result[["s"]], 0.345017)
    expect_equal(model_result[["ik"]], 440.1289906)
    expect_equal(model_result[["im"]], 1399.76963)
    expect_equal(model_result[["w"]], 1.180362267)
  } else {
    skip(paste("Unsupported operating system:", os_name))
  }
})

test_that("test-eilers_peeters_etr_I control plot 20240925.csv", {
  test_data_file <- file.path(getwd(), "data", "20240925.csv")
  data <- read_dual_pam_data(test_data_file)
  model_result <- eilers_peeters_generate_regression_ETR_I(data)

  expect_no_warning(
    print(
      plot_control(
        data,
        model_result,
        "eilers_peeters ETR I 20240925.csv",
        color_eilers_peeters
      )
    )
  )
})

test_that("test-eilers_peeters_etr_I generate regression modified 20240925.csv", {
  test_data_file <- file.path(getwd(), "data", "20240925.csv")
  data <- read_dual_pam_data(test_data_file)
  model_result <- eilers_peeters_generate_regression_ETR_I(data)

  expect_no_warning(
    model_result <- eilers_peeters_modified(model_result)
  )

  os_name <- Sys.info()[["sysname"]]

  if (os_name == "Linux") {
    os_release <- readLines("/etc/os-release")
    distro_line <- grep("^ID=", os_release, value = TRUE)
    distro <- sub("^ID=", "", distro_line)

    if (distro == "debian" || distro == "ubuntu") {
      expect_equal(model_result[["sdiff"]], 53.4454062)
      #expect_equal(model_result[["a"]], 0.000001479)
      #expect_equal(model_result[["b"]], 0.002444096)
      expect_equal(model_result[["c"]], 2.8984079)
      expect_equal(model_result[["d"]], NA_real_)
      expect_equal(model_result[["alpha"]], 0.345017)
      expect_equal(model_result[["beta"]], NA_real_)
      expect_equal(model_result[["etrmax_with_photoinhibition"]], 151.851986)
      expect_equal(model_result[["etrmax_without_photoinhibition"]], NA_real_)
      expect_equal(model_result[["ik_with_photoinhibition"]], 440.1289906)
      expect_equal(model_result[["ik_without_photoinhibition"]], NA_real_)
      expect_equal(model_result[["im_with_photoinhibition"]], 1399.76963)
      expect_equal(model_result[["w"]], 1.180362267)
      expect_equal(model_result[["ib"]], NA_real_)
      expect_equal(model_result[["etrmax_with_without_ratio"]], NA_real_)
    } else {
      skip(paste("Skipping test on unsupported Linux distribution:", distro))
    }
  } else if (os_name == "Windows") {
    expect_equal(model_result[["sdiff"]], 53.4454062)
    #expect_equal(model_result[["a"]], 0.000001479)
    #expect_equal(model_result[["b"]], 0.002444096)
    expect_equal(model_result[["c"]], 2.8984079)
    expect_equal(model_result[["d"]], NA_real_)
    expect_equal(model_result[["alpha"]], 0.345017)
    expect_equal(model_result[["beta"]], NA_real_)
    expect_equal(model_result[["etrmax_with_photoinhibition"]], 151.851986)
    expect_equal(model_result[["etrmax_without_photoinhibition"]], NA_real_)
    expect_equal(model_result[["ik_with_photoinhibition"]], 440.1289906)
    expect_equal(model_result[["ik_without_photoinhibition"]], NA_real_)
    expect_equal(model_result[["im_with_photoinhibition"]], 1399.76963)
    expect_equal(model_result[["w"]], 1.180362267)
    expect_equal(model_result[["ib"]], NA_real_)
    expect_equal(model_result[["etrmax_with_without_ratio"]], NA_real_)
  } else {
    skip(paste("Unsupported operating system:", os_name))
  }
})



test_that("test-eilers_peeters_etr_I modified control plot 20240925.csv", {
  test_data_file <- file.path(getwd(), "data", "20240925.csv")
  data <- read_dual_pam_data(test_data_file)
  model_result <- eilers_peeters_generate_regression_ETR_I(data)
  model_result <- eilers_peeters_modified(model_result)

  expect_no_warning(
    print(
      plot_control(
        data,
        model_result,
        "eilers_peeters ETR I modified 20240925.csv",
        color_eilers_peeters
      )
    )
  )
})
