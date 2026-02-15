test_that("compare_regression_models etr II - linux", {
  skip_if_not(is_debian_or_ubuntu())

  test_data_dir <- testthat::test_path("data", "bulk")
  result <- compare_regression_models_ETR_II(test_data_dir, read_dual_pam_data)

  expect_equal(result[["eilers_peeters"]], 37)
  expect_equal(result[["platt"]], 20)
  expect_equal(result[["vollenweider"]], 39)
  expect_equal(result[["walsby"]], 6)
})

test_that("compare_regression_models etr II", {
  test_data_dir <- testthat::test_path("data", "bulk")
  result <- compare_regression_models_ETR_II(test_data_dir, read_dual_pam_data)
  expect_named(result, c("eilers_peeters", "platt", "vollenweider", "walsby"), ignore.order = TRUE)
})
