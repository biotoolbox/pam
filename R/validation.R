validate_data <- function(data) {
  library(data.table)

  if (is.null(data)) {
    stop("data is null")
  }

  if (!is.data.table(data)) {
    stop("data is not a valid data.table")
  }

  if (nrow(data) < 2) {
    stop("no data rows")
  }

  if (ncol(data) == 0) {
    stop("no cols in data")
  }

  if (!"ID" %in% colnames(data)) {
    stop("required col 'ID' not found")
  }

  if (!"PAR" %in% colnames(data)) {
    stop("required col 'PAR' not found")
  }

  if (!"Y.I." %in% colnames(data) && !"Y.II." %in% colnames(data)) {
    stop("required col 'Y(I)' and 'Y(II)' not found")
  }

  if (!"Action" %in% colnames(data)) {
    stop("required col 'Action' not found")
  }

  if (!"Date" %in% colnames(data)) {
    stop("required col 'Date' not found")
  }

  if (!"Time" %in% colnames(data)) {
    stop("required col 'Time' not found")
  }
}

validate_regression_data <- function(regression_data) {
  library(data.table)

  if (is.null(regression_data)) {
    stop("is null")
  }

  if (!is.data.table(regression_data)) {
    stop("not a valid data.table")
  }

  if (nrow(regression_data) < 2) {
    stop("no regression_data rows")
  }

  if (ncol(regression_data) != 2) {
    stop("regression data got more or less then two columns")
  }

  if (!"PAR" %in% colnames(regression_data)) {
    stop("required col 'PAR' not found")
  }

  if (!"prediction" %in% colnames(regression_data)) {
    stop("required col 'prediction' not found")
  }
}

validate_etr_type <- function(etr_type) {
  if (etr_type != etr_I_type && etr_type != etr_II_type) {
    stop("etr type is not valid")
  }
}
