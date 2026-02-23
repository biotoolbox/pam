test_that("test-eilers_peeters_etr_II junior_pam_20250613.csv.csv - linux", {
  skip_if_not(is_debian_or_ubuntu())

  test_data_file <- testthat::test_path("data", "junior_pam_20250613.csv")
  data <- read_junior_pam_data(test_data_file)
  model_result <- eilers_peeters_generate_regression_ETR_II(data)

  expect_equal(model_result[["residual_sum_of_squares"]], 18.5975058)
  # expect_equal(model_result[["a"]], 0.000001594)
  expect_equal(model_result[["b"]], 0.01213671)
  expect_equal(model_result[["c"]], 2.77837046)
  expect_equal(model_result[["pm"]], 61.1793644)
  expect_equal(model_result[["s"]], 0.35992320)
  expect_equal(model_result[["ik"]], 169.978939)
  expect_equal(model_result[["im"]], 1320.307821)
  expect_equal(model_result[["w"]], 5.7674788)
})

test_that("test-eilers_peeters_etr_II junior_pam_20250613.csv.csv", {
  test_data_file <- testthat::test_path("data", "junior_pam_20250613.csv")
  data <- read_junior_pam_data(test_data_file)
  model_result <- eilers_peeters_generate_regression_ETR_II(data)
  expect_no_error(validate_model_result(model_result))
})

test_that("test-eilers_peeters_etr_II modified junior_pam_20250613.csv.csv - linux", {
  skip_if_not(is_debian_or_ubuntu())
  test_data_file <- testthat::test_path("data", "junior_pam_20250613.csv")
  data <- read_junior_pam_data(test_data_file)
  model_result <- eilers_peeters_generate_regression_ETR_II(data)
  model_result <- eilers_peeters_modified(model_result)

  expect_equal(model_result[["residual_sum_of_squares"]], 18.5975058)
  # expect_equal(model_result[["a"]], 0.000001594)
  expect_equal(model_result[["b"]], 0.01213671)
  expect_equal(model_result[["c"]], 2.77837046)
  expect_equal(model_result[["d"]], NA_real_)
  expect_equal(model_result[["alpha"]], 0.35992320)
  expect_equal(model_result[["beta"]], NA_real_)
  expect_equal(model_result[["etrmax_with_photoinhibition"]], 61.1793644)
  expect_equal(model_result[["etrmax_without_photoinhibition"]], NA_real_)
  expect_equal(model_result[["ik_with_photoinhibition"]], 169.978939)
  expect_equal(model_result[["ik_without_photoinhibition"]], NA_real_)
  expect_equal(model_result[["im_with_photoinhibition"]], 1320.307821)
  expect_equal(model_result[["w"]], 5.7674788)
  expect_equal(model_result[["ib"]], NA_real_)
  expect_equal(model_result[["etrmax_with_without_ratio"]], NA_real_)
})

test_that("test-eilers_peeters_etr_II modified junior_pam_20250613.csv.csv", {
  test_data_file <- testthat::test_path("data", "junior_pam_20250613.csv")
  data <- read_junior_pam_data(test_data_file)

  model_result <- eilers_peeters_generate_regression_ETR_II(data)
  expect_no_error(validate_model_result(model_result))

  model_result <- eilers_peeters_modified(model_result)
  expect_no_error(validate_modified_model_result(model_result))
})

test_that("test-eilers_peeters_etr_II modified control plot junior_pam_20250613.csv", {
  test_data_file <- testthat::test_path("data", "junior_pam_20250613.csv")
  data <- read_junior_pam_data(test_data_file)
  model_result <- eilers_peeters_generate_regression_ETR_II(data)
  model_result <- model_result <- eilers_peeters_modified(model_result)

  plot <- plot_control(
    data,
    model_result,
    "eilers_peeters ETR II modified junior_pam_20250613.csv",
    color_eilers_peeters
  )
  expect_s3_class(plot, "ggplot")
  expect_gt(length(plot$layers), 0)

  out <- file.path("results", "test-eilers_peeters_etr_II modified control plot junior_pam_20250613.jpg")
  ggplot2::ggsave(out, create.dir = TRUE, plot = plot, units = "px", width = 1000, height = 1000, dpi = 100, limitsize = FALSE)
  expect_true(file.exists(out))
})
