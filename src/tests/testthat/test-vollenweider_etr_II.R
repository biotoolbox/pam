test_that("test-vollenweider_etr_II generate regression 20240925.csv", {
  test_data_file <- file.path(getwd(), "data", "20240925.csv")
  data <- read_dual_pam_data(test_data_file)
  model_result <- vollenweider_generate_regression_ETR_II(data)

  os_name <- Sys.info()[["sysname"]]

  if (os_name == "Linux") {
    os_release <- readLines("/etc/os-release")
    distro_line <- grep("^ID=", os_release, value = TRUE)
    distro <- sub("^ID=", "", distro_line)

    if (distro == "debian" || distro == "ubuntu") {
      expect_equal(model_result[["sdiff"]], 2.5104818)
      expect_equal(model_result[["pmax"]], 53.404027)
      #expect_equal(model_result[["a"]], 0.002845961)
      #expect_equal(model_result[["alpha"]], -0.000378034)
      expect_equal(model_result[["n"]], 2.34641)
      expect_equal(model_result[["ik"]], 351.37520)
      expect_equal(model_result[["popt"]], 44.16714)
      expect_equal(model_result[["iik"]], 290.60051)
      expect_equal(model_result[["pmax_popt_and_ik_iik_ratio"]], 1.209134835)
    } else {
      skip(paste("Skipping test on unsupported Linux distribution:", distro))
    }
  } else if (os_name == "Windows") {
    expect_equal(model_result[["sdiff"]], 2.477197461)
    expect_equal(model_result[["pmax"]], 53.4558073)
    #expect_equal(model_result[["a"]], 0.002845961)
    #expect_equal(model_result[["alpha"]], -0.000378034)
    expect_equal(model_result[["n"]], 2.195311080)
    expect_equal(model_result[["ik"]], 351.808177)
    expect_equal(model_result[["popt"]], 44.1662558)
    expect_equal(model_result[["iik"]], 290.670944)
    expect_equal(model_result[["pmax_popt_and_ik_iik_ratio"]], 1.21033142)
  } else {
    skip(paste("Unsupported operating system:", os_name))
  }
})

test_that("test-vollenweider_etr_II control plot 20240925.csv", {
  test_data_file <- file.path(getwd(), "data", "20240925.csv")
  data <- read_dual_pam_data(test_data_file)
  model_result <- vollenweider_generate_regression_ETR_II(data)

  expect_no_warning(
    print(
      plot_control(
        data,
        model_result,
        "vollenweider ETR II 20240925.csv",
        color_vollenweider
      )
    )
  )
})

test_that("test-vollenweider_etr_II generate regression modified 20240925.csv", {
  test_data_file <- file.path(getwd(), "data", "20240925.csv")
  data <- read_dual_pam_data(test_data_file)
  model_result <- vollenweider_generate_regression_ETR_II(data)
  model_result <- vollenweider_modified(model_result)

  os_name <- Sys.info()[["sysname"]]

  if (os_name == "Linux") {
    os_release <- readLines("/etc/os-release")
    distro_line <- grep("^ID=", os_release, value = TRUE)
    distro <- sub("^ID=", "", distro_line)

    if (distro == "debian" || distro == "ubuntu") {
      expect_equal(model_result[["sdiff"]], 2.5104818)
      expect_equal(model_result[["a"]], 53.404027)
      #expect_equal(model_result[["b"]], 0.002845961)
      #expect_equal(model_result[["c"]], -0.000378034)
      expect_equal(model_result[["d"]], 2.34641)
      expect_equal(model_result[["alpha"]], 0.15198576)
      expect_equal(model_result[["beta"]], NA_real_)
      expect_equal(model_result[["etrmax_with_photoinhibition"]], 44.16714)
      expect_equal(model_result[["etrmax_without_photoinhibition"]], 53.404027)
      expect_equal(model_result[["ik_with_photoinhibition"]], 290.60051)
      expect_equal(model_result[["ik_without_photoinhibition"]], 351.37520)
      expect_equal(model_result[["im_with_photoinhibition"]], 757.0)
      expect_equal(model_result[["w"]], NA_real_)
      expect_equal(model_result[["ib"]], NA_real_)
      expect_equal(model_result[["etrmax_with_without_ratio"]], 1.209134835)
    } else {
      skip(paste("Skipping test on unsupported Linux distribution:", distro))
    }
  } else if (os_name == "Windows") {
    expect_equal(model_result[["sdiff"]], 2.477197461)
    expect_equal(model_result[["a"]], 53.4558073)
    #expect_equal(model_result[["b"]], 0.002845961)
    #expect_equal(model_result[["c"]], -0.000378034)
    expect_equal(model_result[["d"]], 2.195311080)
    expect_equal(model_result[["alpha"]], 0.15194589)
    expect_equal(model_result[["beta"]], NA_real_)
    expect_equal(model_result[["etrmax_with_photoinhibition"]], 44.1662558)
    expect_equal(model_result[["etrmax_without_photoinhibition"]], 53.4558073)
    expect_equal(model_result[["ik_with_photoinhibition"]], 290.670944)
    expect_equal(model_result[["ik_without_photoinhibition"]], 351.808177)
    expect_equal(model_result[["im_with_photoinhibition"]], 756.0)
    expect_equal(model_result[["w"]], NA_real_)
    expect_equal(model_result[["ib"]], NA_real_)
    expect_equal(model_result[["etrmax_with_without_ratio"]], 1.21033142)
  } else {
    skip(paste("Unsupported operating system:", os_name))
  }
})



test_that("test-vollenweider_etr_II modified control plot 20240925.csv", {
  test_data_file <- file.path(getwd(), "data", "20240925.csv")
  data <- read_dual_pam_data(test_data_file)
  model_result <- vollenweider_generate_regression_ETR_II(data)
  model_result <- vollenweider_modified(model_result)

  expect_no_warning(
    print(
      plot_control(
        data,
        model_result,
        "vollenweider ETR II modified 20240925.csv",
        color_vollenweider
      )
    )
  )
})