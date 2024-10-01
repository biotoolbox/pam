calculate_sdiff <- function(data, etr_regression_data, etr_type) {
  validate_data(data)
  validate_regression_data(etr_regression_data)
  validate_etr_type(etr_type)

  final_sdiff <- 0
  for (i in seq_len(nrow(data))) {
    row <- data[i, ]
    real_etr <- row[[etr_type]]
    predicted_etr <- etr_regression_data[etr_regression_data$PAR == row$PAR, ][[prediction_name]]

    sdiff <- (predicted_etr - real_etr)^2
    final_sdiff <- final_sdiff + sdiff
  }

  return(final_sdiff)
}

remove_det_row_by_etr <- function(data, etr_type) {
  library(dplyr)
  validate_data(data)
  validate_etr_type(etr_type)

  if (etr_type == etr_I_type) {
    data <- data %>% filter(data$Action != "Fm-Det.")
    if (length(data[data$Action == "Pm.-Det.", ]) == 0) {
      stop("Pm.-Det. is required but not present")
    }
  } else {
    data <- data %>% filter(data$Action != "Pm.-Det.")
    if (length(data[data$Action == "Fm-Det.", ]) == 0) {
      stop("Fm-Det. is required but not present")
    }
  }

  return(data)
}

create_regression_data <- function(pars, predictions) {
  library(data.table)

  if (!is.vector(pars)) {
    stop("pars is not a valid vector")
  }

  if (!is.vector(predictions)) {
    stop("predictions is not a valid vector")
  }

  if (length(pars) != length(predictions)) {
    stop("pars and predictions need to be of the same length")
  }

  regression_data <- data.table(
    "PAR" = pars,
    "prediction" = predictions
  )
  return(regression_data)
}
