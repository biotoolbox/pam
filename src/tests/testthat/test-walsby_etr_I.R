test_that("test-walsby_etr_I generate regression 20240925.csv", {
  test_data_file <- file.path(getwd(), "data", "20240925.csv")
  data <- read_dual_pam_data(test_data_file)
  model_result <- walsby_generate_regression_ETR_I(data)

  os_name <- Sys.info()[["sysname"]]

  if (os_name == "Linux") {
    os_release <- readLines("/etc/os-release")
    distro_line <- grep("^ID=", os_release, value = TRUE)
    distro <- sub("^ID=", "", distro_line)

    if (distro == "debian" || distro == "ubuntu") {
      expect_equal(model_result[["sdiff"]], 55.5823146)
      expect_equal(model_result[["etr_max"]], 221.23782)
      expect_equal(model_result[["alpha"]], 0.387249932)
      expect_equal(model_result[["beta"]], -0.035964253)
    } else {
      skip(paste("Skipping test on unsupported Linux distribution:", distro))
    }
  } else if (os_name == "Windows") {
    expect_equal(model_result[["sdiff"]], 55.5823146)
    expect_equal(model_result[["etr_max"]], 221.23782)
    expect_equal(model_result[["alpha"]], 0.387249932)
    expect_equal(model_result[["beta"]], -0.035964253)
  } else {
    skip(paste("Unsupported operating system:", os_name))
  }
})

test_that("test-walsby_etr_I control plot 20240925.csv", {
  test_data_file <- file.path(getwd(), "data", "20240925.csv")
  data <- read_dual_pam_data(test_data_file)
  model_result <- walsby_generate_regression_ETR_I(data)

  expect_no_warning(
    print(
      plot_control(
        data,
        model_result,
        "walsby ETR I 20240925.csv",
        color_platt
      )
    )
  )
})

test_that("test-walsby_etr_I generate regression modified 20240925.csv", {
  test_data_file <- file.path(getwd(), "data", "20240925.csv")
  data <- read_dual_pam_data(test_data_file)
  model_result <- walsby_generate_regression_ETR_I(data)
  model_result <- walsby_modified(model_result)

  os_name <- Sys.info()[["sysname"]]

  if (os_name == "Linux") {
    os_release <- readLines("/etc/os-release")
    distro_line <- grep("^ID=", os_release, value = TRUE)
    distro <- sub("^ID=", "", distro_line)

    if (distro == "debian" || distro == "ubuntu") {
      expect_equal(model_result[["sdiff"]], 55.5823146)
      expect_equal(model_result[["a"]], 221.23782)
      expect_equal(model_result[["b"]], 0.387249932)
      expect_equal(model_result[["c"]], -0.035964253)
      expect_equal(model_result[["d"]], NA_real_)
      expect_equal(model_result[["alpha"]], 0.387249932)
      expect_equal(model_result[["beta"]], -0.035964253)
      expect_equal(model_result[["etrmax_with_photoinhibition"]], 151.8614464)
      expect_equal(model_result[["etrmax_without_photoinhibition"]], 221.23782)
      expect_equal(model_result[["ik_with_photoinhibition"]], 392.15358)
      expect_equal(model_result[["ik_without_photoinhibition"]], 571.3050)
      expect_equal(model_result[["im_with_photoinhibition"]], 1358.0)
      expect_equal(model_result[["w"]], NA_real_)
      expect_equal(model_result[["ib"]], NA_real_)
      expect_equal(model_result[["etrmax_with_without_ratio"]], 1.456839939)
    } else {
      skip(paste("Skipping test on unsupported Linux distribution:", distro))
    }
  } else if (os_name == "Windows") {
    expect_equal(model_result[["sdiff"]], 55.5823146)
    expect_equal(model_result[["a"]], 221.23782)
    expect_equal(model_result[["b"]], 0.387249932)
    expect_equal(model_result[["c"]], -0.035964253)
    expect_equal(model_result[["d"]], NA_real_)
    expect_equal(model_result[["alpha"]], 0.387249932)
    expect_equal(model_result[["beta"]], -0.035964253)
    expect_equal(model_result[["etrmax_with_photoinhibition"]], 151.8614464)
    expect_equal(model_result[["etrmax_without_photoinhibition"]], 221.23782)
    expect_equal(model_result[["ik_with_photoinhibition"]], 392.15358)
    expect_equal(model_result[["ik_without_photoinhibition"]], 571.3050)
    expect_equal(model_result[["im_with_photoinhibition"]], 1358.0)
    expect_equal(model_result[["w"]], NA_real_)
    expect_equal(model_result[["ib"]], NA_real_)
    expect_equal(model_result[["etrmax_with_without_ratio"]], 1.456839939)
  } else {
    skip(paste("Unsupported operating system:", os_name))
  }
})



test_that("test-walsby_etr_I modified control plot 20240925.csv", {
  test_data_file <- file.path(getwd(), "data", "20240925.csv")
  data <- read_dual_pam_data(test_data_file)
  model_result <- walsby_generate_regression_ETR_I(data)
  model_result <- walsby_modified(model_result)

  expect_no_warning(
    print(
      plot_control(
        data,
        model_result,
        "walsby ETR I modified 20240925.csv",
        color_platt
      )
    )
  )
})
