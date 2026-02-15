test_that("compare_regression_models etr I - linux", {
  skip_if_not(is_debian_or_ubuntu())
  test_data_dir <- testthat::test_path("data", "bulk")
  result <- compare_regression_models_ETR_I(test_data_dir, read_dual_pam_data)

  expect_equal(result[["eilers_peeters"]], 13)
  expect_equal(result[["platt"]], 19)
  expect_equal(result[["vollenweider"]], 33)
  expect_equal(result[["walsby"]], 25)
})

test_that("compare_regression_models etr I", {
  test_data_dir <- testthat::test_path("data", "bulk")
  result <- compare_regression_models_ETR_I(test_data_dir, read_dual_pam_data)
  expect_named(result, c("eilers_peeters", "platt", "vollenweider", "walsby"), ignore.order = TRUE)
})
