test_that("test-eilers_peeters_etr_II generate regression junior_pam_20250613.csv.csv", {
  test_data_file <- file.path(getwd(), "data_junior_pam", "junior_pam_20250613.csv")
  data <- read_junior_pam_data(test_data_file)
  model_result <- eilers_peeters_generate_regression_ETR_II(data)

  os_name <- Sys.info()[["sysname"]]

  if (os_name == "Linux") {
    os_release <- readLines("/etc/os-release")
    distro_line <- grep("^ID_LIKE=", os_release, value = TRUE)
    distro <- sub("^ID_LIKE=", "", distro_line)

    if (grepl("ubuntu", distro) || grepl("debian", distro)) {
      expect_equal(model_result[["sdiff"]], 18.5975058)
      #expect_equal(model_result[["a"]], 0.000001594)
      expect_equal(model_result[["b"]], 0.01213671)
      expect_equal(model_result[["c"]], 2.77837046)
      expect_equal(model_result[["pm"]], 61.1793644)
      expect_equal(model_result[["s"]], 0.35992320)
      expect_equal(model_result[["ik"]], 169.978939)
      expect_equal(model_result[["im"]], 1320.3078)
      expect_equal(model_result[["w"]], 5.7674788)
    } else {
      skip(paste("Skipping test on unsupported Linux distribution:", distro))
    }
  } else if (os_name == "Windows") {
     expect_equal(model_result[["sdiff"]], 18.5975058)
      #expect_equal(model_result[["a"]], 0.000001594)
      expect_equal(model_result[["b"]], 0.01213671)
      expect_equal(model_result[["c"]], 2.77837046)
      expect_equal(model_result[["pm"]], 61.1793644)
      expect_equal(model_result[["s"]], 0.35992320)
      expect_equal(model_result[["ik"]], 169.978939)
      expect_equal(model_result[["im"]], 1320.3078)
      expect_equal(model_result[["w"]], 5.7674788)
  } else {
    skip(paste("Unsupported operating system:", os_name))
  }
})

test_that("test-eilers_peeters_etr_II junior_pam_20250613.csv.csv", {
  test_data_file <- file.path(getwd(), "data_junior_pam", "junior_pam_20250613.csv")
  data <- read_junior_pam_data(test_data_file)
  model_result <- eilers_peeters_generate_regression_ETR_II(data)

  expect_no_warning(
    model_result <- eilers_peeters_modified(model_result)
  )

  os_name <- Sys.info()[["sysname"]]

  if (os_name == "Linux") {
    os_release <- readLines("/etc/os-release")
    distro_line <- grep("^ID_LIKE=", os_release, value = TRUE)
    distro <- sub("^ID_LIKE=", "", distro_line)

    if (grepl("ubuntu", distro) || grepl("debian", distro)) {
      expect_equal(model_result[["sdiff"]], 18.5975058)
      #expect_equal(model_result[["a"]], 0.000001594)
      expect_equal(model_result[["b"]], 0.01213671)
      expect_equal(model_result[["c"]], 2.77837046)
      expect_equal(model_result[["d"]], NA_real_)
      expect_equal(model_result[["alpha"]], 0.35992320)
      expect_equal(model_result[["beta"]], NA_real_)
      expect_equal(model_result[["etrmax_with_photoinhibition"]], 61.1793644)
      expect_equal(model_result[["etrmax_without_photoinhibition"]], NA_real_)
      expect_equal(model_result[["ik_with_photoinhibition"]], 169.978939)
      expect_equal(model_result[["ik_without_photoinhibition"]], NA_real_)
      expect_equal(model_result[["im_with_photoinhibition"]], 1320.3078)
      expect_equal(model_result[["w"]], 5.7674788)
      expect_equal(model_result[["ib"]], NA_real_)
      expect_equal(model_result[["etrmax_with_without_ratio"]], NA_real_)
    } else {
      skip(paste("Skipping test on unsupported Linux distribution:", distro))
    }
  } else if (os_name == "Windows") {
      expect_equal(model_result[["sdiff"]], 18.5975058)
      #expect_equal(model_result[["a"]], 0.000001594)
      expect_equal(model_result[["b"]], 0.01213671)
      expect_equal(model_result[["c"]], 2.77837046)
      expect_equal(model_result[["d"]], NA_real_)
      expect_equal(model_result[["alpha"]], 0.35992320)
      expect_equal(model_result[["beta"]], NA_real_)
      expect_equal(model_result[["etrmax_with_photoinhibition"]], 61.1793644)
      expect_equal(model_result[["etrmax_without_photoinhibition"]], NA_real_)
      expect_equal(model_result[["ik_with_photoinhibition"]], 169.978939)
      expect_equal(model_result[["ik_without_photoinhibition"]], NA_real_)
      expect_equal(model_result[["im_with_photoinhibition"]], 1320.3078)
      expect_equal(model_result[["w"]], 5.7674788)
      expect_equal(model_result[["ib"]], NA_real_)
      expect_equal(model_result[["etrmax_with_without_ratio"]], NA_real_)
  } else {
    skip(paste("Unsupported operating system:", os_name))
  }
})



test_that("test-eilers_peeters_etr_II modified control plot junior_pam_20250613.csv", {
  test_data_file <- file.path(getwd(), "data_junior_pam", "junior_pam_20250613.csv")
  data <- read_junior_pam_data(test_data_file)
  model_result <- eilers_peeters_generate_regression_ETR_II(data)
  model_result <- model_result <- eilers_peeters_modified(model_result)

  expect_no_warning(
    print(
      plot <- plot_control(
        data,
        model_result,
        "eilers_peeters ETR II modified junior_pam_20250613.csv",
        color_eilers_peeters
      )
    )
  )
})
