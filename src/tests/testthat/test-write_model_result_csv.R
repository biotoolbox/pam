test_that("test-write_model_result_csv - walsby_modified - 20240925.csv", {
  test_data_file <- testthat::test_path("data", "20240925.csv")
  data <- read_dual_pam_data(test_data_file)
  model_result <- walsby_generate_regression_ETR_II(data)
  model_result <- walsby_modified(model_result)

  result_dir <- testthat::test_path("results")
  dir.create(result_dir, showWarnings = FALSE)

  model_result_csv_path <- file.path(result_dir, "20240925_model_result.csv")
  raw_data_csv_path <- file.path(result_dir, "20240925_raw_data.csv")
  regression_data_csv_path <- file.path(result_dir, "20240925_regression_data.csv")
  unlink(model_result_csv_path)
  unlink(raw_data_csv_path)
  unlink(regression_data_csv_path)

  write_model_result_csv(
    result_dir,
    "20240925",
    data,
    model_result
  )

  model_result_csv <- read.csv(
    file = model_result_csv_path,
    colClasses = "numeric"
  )
  expect_equal(model_result_csv$a, model_result$a)
  expect_equal(model_result_csv$b, model_result$b)
  expect_equal(model_result_csv$c, model_result$c)
  expect_equal(model_result_csv$d, model_result$d)
  expect_equal(model_result_csv$alpha, model_result$alpha)
  expect_equal(model_result_csv$beta, model_result$beta)
  expect_equal(model_result_csv$etrmax_with_photoinhibition, model_result$etrmax_with_photoinhibition)
  expect_equal(model_result_csv$etrmax_without_photoinhibition, model_result$etrmax_without_photoinhibition)
  expect_equal(model_result_csv$im_with_photoinhibition, model_result$im_with_photoinhibition)
  expect_equal(model_result_csv$w, model_result$w)
  expect_equal(model_result_csv$ib, model_result$ib)
  expect_equal(model_result_csv$etrmax_with_without_ratio, model_result$etrmax_with_without_ratio)

  raw_data_csv <- read.csv(
    file = raw_data_csv_path,
    colClasses = "numeric"
  )
  expect_equal(as.data.frame(raw_data_csv), as.data.frame(data))

  regression_data_csv <- read.csv(
    file = regression_data_csv_path,
    colClasses = "numeric"
  )
  expect_equal(as.data.frame(regression_data_csv), as.data.frame(model_result$etr_regression_data))
})

test_that("test-write_model_result_csv - walsby - 20240925.csv", {
  test_data_file <- testthat::test_path("data", "20240925.csv")
  data <- read_dual_pam_data(test_data_file)
  model_result <- walsby_generate_regression_ETR_II(data)

  result_dir <- testthat::test_path("results")
  dir.create(result_dir, showWarnings = FALSE)

  model_result_csv_path <- file.path(result_dir, "20240925_model_result.csv")
  raw_data_csv_path <- file.path(result_dir, "20240925_raw_data.csv")
  regression_data_csv_path <- file.path(result_dir, "20240925_regression_data.csv")
  unlink(model_result_csv_path)
  unlink(raw_data_csv_path)
  unlink(regression_data_csv_path)

  write_model_result_csv(
    result_dir,
    "20240925",
    data,
    model_result
  )

  model_result_csv <- read.csv(
    file = model_result_csv_path,
    colClasses = "numeric"
  )
  expect_equal(model_result_csv$a, model_result$a)
  expect_equal(model_result_csv$b, model_result$b)
  expect_equal(model_result_csv$c, model_result$c)
  expect_equal(model_result_csv$d, model_result$d)
  expect_equal(model_result_csv$alpha, model_result$alpha)
  expect_equal(model_result_csv$beta, model_result$beta)
  expect_equal(model_result_csv$etrmax_with_photoinhibition, model_result$etrmax_with_photoinhibition)
  expect_equal(model_result_csv$etrmax_without_photoinhibition, model_result$etrmax_without_photoinhibition)
  expect_equal(model_result_csv$im_with_photoinhibition, model_result$im_with_photoinhibition)
  expect_equal(model_result_csv$w, model_result$w)
  expect_equal(model_result_csv$ib, model_result$ib)
  expect_equal(model_result_csv$etrmax_with_without_ratio, model_result$etrmax_with_without_ratio)

  raw_data_csv <- read.csv(
    file = raw_data_csv_path,
    colClasses = "numeric"
  )
  expect_equal(as.data.frame(raw_data_csv), as.data.frame(data))

  regression_data_csv <- read.csv(
    file = regression_data_csv_path,
    colClasses = "numeric"
  )
  expect_equal(as.data.frame(regression_data_csv), as.data.frame(model_result$etr_regression_data))
})
