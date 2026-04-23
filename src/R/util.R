create_regression_data <- function(pars, predictions) {
  if (!is.vector(pars)) {
    stop("pars is not a valid vector")
  }

  if (!is.vector(predictions)) {
    stop("predictions is not a valid vector")
  }

  if (length(pars) != length(predictions)) {
    stop("pars and predictions need to be of the same length")
  }

  regression_data <- data.table::data.table(
    "par" = pars,
    "prediction" = predictions
  )
  return(regression_data)
}

get_etr_type_from_model_result <- function(model_result) {
  return(model_result[["etr_type"]])
}

get_etr_regression_data_from_model_result <- function(model_result) {
  return(model_result[["etr_regression_data"]])
}

get_residual_sum_of_squares_from_model_result <- function(model_result) {
  return(model_result[["residual_sum_of_squares"]])
}

get_etr_data_for_par_values <- function(data, etr_regression_data, etr_type) {
  validate_intermediate_data(data)
  validate_etr_regression_data(etr_regression_data)
  validate_etr_type(etr_type)

  result <- data.table::data.table(par = numeric(), measured_etr = numeric(), predicted_etr = numeric())

  for (i in seq_len(nrow(data))) {
    row <- data[i, ]
    par <- row$par
    measured_etr <- row[[etr_type]]
    predicted_etr <- etr_regression_data[etr_regression_data$par == par, ][[prediction_name]]
    result_row <- list(par = par, measured_etr = measured_etr, predicted_etr = predicted_etr)
    result <- rbind(result, result_row)
  }
  return(result)
}

root_mean_squared_error <- function(measured_predicted_etr_data) {
  measured_etr <- measured_predicted_etr_data$measured_etr
  predicted_etr <- measured_predicted_etr_data$predicted_etr
  root_mean_squared_error <- Metrics::rmse(measured_etr, predicted_etr)
  return(root_mean_squared_error)
}

relative_root_mean_squared_error <- function(measured_predicted_etr_data) {
  measured_etr <- measured_predicted_etr_data$measured_etr
  predicted_etr <- measured_predicted_etr_data$predicted_etr
  root_mean_squared_error <- Metrics::rmse(measured_etr, predicted_etr)
  relative_root_mean_squared_error <- root_mean_squared_error / mean(predicted_etr)
  return(relative_root_mean_squared_error)
}

calc_etr <- function(yield, par, etr_factor, p_ratio) {
  if (is.na(yield)) {
    return(NA_real_)
  }

  if (!is.numeric(yield)) {
    stop("yield is not numeric")
  }

  if (!is.numeric(par)) {
    stop("par is not numeric")
  }

  if (!is.numeric(etr_factor)) {
    stop("etr_factor is not numeric")
  }

  if (!is.numeric(p_ratio)) {
    stop("p_ratio is not numeric")
  }

  return(yield * par * etr_factor * p_ratio)
}
