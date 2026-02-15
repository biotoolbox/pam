test_that("test-get_etr_data_for_par_values.R", {
  test_data_file <- testthat::test_path("data", "20240925.csv")
  data <- read_dual_pam_data(test_data_file)
  model_result <- walsby_generate_regression_ETR_II(data)
  etr_regression_data <- get_etr_regression_data_from_model_result(model_result)
  result <- get_etr_data_for_par_values(data, etr_regression_data, etr_2_type)

  par <- result[[PAR_name]]
  expect_equal(par[1], data[[PAR_name]][1])
  expect_equal(par[2], data[[PAR_name]][2])
  expect_equal(par[3], data[[PAR_name]][3])
  expect_equal(par[4], data[[PAR_name]][4])
  expect_equal(par[5], data[[PAR_name]][5])
  expect_equal(par[6], data[[PAR_name]][6])
  expect_equal(par[7], data[[PAR_name]][7])
  expect_equal(par[8], data[[PAR_name]][8])
  expect_equal(par[9], data[[PAR_name]][9])
  expect_equal(par[10], data[[PAR_name]][10])
  expect_equal(par[11], data[[PAR_name]][11])
  expect_equal(par[12], data[[PAR_name]][12])
  expect_equal(par[13], data[[PAR_name]][13])
  expect_equal(par[14], data[[PAR_name]][14])
  expect_equal(par[15], data[[PAR_name]][15])
  expect_equal(par[16], data[[PAR_name]][16])
  expect_equal(par[17], data[[PAR_name]][17])

  measured_etr <- result$measured_etr
  expect_equal(measured_etr[1], data[[etr_2_type]][1])
  expect_equal(measured_etr[2], data[[etr_2_type]][2])
  expect_equal(measured_etr[3], data[[etr_2_type]][3])
  expect_equal(measured_etr[3], data[[etr_2_type]][3])
  expect_equal(measured_etr[4], data[[etr_2_type]][4])
  expect_equal(measured_etr[5], data[[etr_2_type]][5])
  expect_equal(measured_etr[6], data[[etr_2_type]][6])
  expect_equal(measured_etr[7], data[[etr_2_type]][7])
  expect_equal(measured_etr[8], data[[etr_2_type]][8])
  expect_equal(measured_etr[9], data[[etr_2_type]][9])
  expect_equal(measured_etr[10], data[[etr_2_type]][10])
  expect_equal(measured_etr[11], data[[etr_2_type]][11])
  expect_equal(measured_etr[12], data[[etr_2_type]][12])
  expect_equal(measured_etr[13], data[[etr_2_type]][13])
  expect_equal(measured_etr[14], data[[etr_2_type]][14])
  expect_equal(measured_etr[15], data[[etr_2_type]][15])
  expect_equal(measured_etr[16], data[[etr_2_type]][16])
  expect_equal(measured_etr[17], data[[etr_2_type]][17])

  predicted_etr <- result$predicted_etr
  expect_equal(
    predicted_etr[1],
    etr_regression_data[etr_regression_data[[PAR_name]] == result[[PAR_name]][1], ][[prediction_name]]
  )
  expect_equal(
    predicted_etr[2],
    etr_regression_data[etr_regression_data[[PAR_name]] == result[[PAR_name]][2], ][[prediction_name]]
  )
  expect_equal(
    predicted_etr[3],
    etr_regression_data[etr_regression_data[[PAR_name]] == result[[PAR_name]][3], ][[prediction_name]]
  )
  expect_equal(
    predicted_etr[4],
    etr_regression_data[etr_regression_data[[PAR_name]] == result[[PAR_name]][4], ][[prediction_name]]
  )
  expect_equal(
    predicted_etr[5],
    etr_regression_data[etr_regression_data[[PAR_name]] == result[[PAR_name]][5], ][[prediction_name]]
  )
  expect_equal(
    predicted_etr[6],
    etr_regression_data[etr_regression_data[[PAR_name]] == result[[PAR_name]][6], ][[prediction_name]]
  )
  expect_equal(
    predicted_etr[7],
    etr_regression_data[etr_regression_data[[PAR_name]] == result[[PAR_name]][7], ][[prediction_name]]
  )
  expect_equal(
    predicted_etr[8],
    etr_regression_data[etr_regression_data[[PAR_name]] == result[[PAR_name]][8], ][[prediction_name]]
  )
  expect_equal(
    predicted_etr[9],
    etr_regression_data[etr_regression_data[[PAR_name]] == result[[PAR_name]][9], ][[prediction_name]]
  )
  expect_equal(
    predicted_etr[10],
    etr_regression_data[etr_regression_data[[PAR_name]] == result[[PAR_name]][10], ][[prediction_name]]
  )
  expect_equal(
    predicted_etr[11],
    etr_regression_data[etr_regression_data[[PAR_name]] == result[[PAR_name]][11], ][[prediction_name]]
  )
  expect_equal(
    predicted_etr[12],
    etr_regression_data[etr_regression_data[[PAR_name]] == result[[PAR_name]][12], ][[prediction_name]]
  )
  expect_equal(
    predicted_etr[13],
    etr_regression_data[etr_regression_data[[PAR_name]] == result[[PAR_name]][13], ][[prediction_name]]
  )
  expect_equal(
    predicted_etr[14],
    etr_regression_data[etr_regression_data[[PAR_name]] == result[[PAR_name]][14], ][[prediction_name]]
  )
  expect_equal(
    predicted_etr[15],
    etr_regression_data[etr_regression_data[[PAR_name]] == result[[PAR_name]][15], ][[prediction_name]]
  )
  expect_equal(
    predicted_etr[16],
    etr_regression_data[etr_regression_data[[PAR_name]] == result[[PAR_name]][16], ][[prediction_name]]
  )
  expect_equal(
    predicted_etr[17],
    etr_regression_data[etr_regression_data[[PAR_name]] == result[[PAR_name]][17], ][[prediction_name]]
  )
})
