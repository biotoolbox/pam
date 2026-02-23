validate_data <- function(data) {
  if (is.null(data)) {
    stop("data is null")
  }

  if (!data.table::is.data.table(data)) {
    stop("data is not a valid data.table")
  }

  if (!"par" %in% colnames(data)) {
    stop("required col 'par' not found")
  }

  if (!"yield_1" %in% colnames(data)) {
    stop("required col 'yield_1' not found")
  }

  if (!"yield_2" %in% colnames(data)) {
    stop("required col 'yield_2' not found")
  }

  if (!"etr_1" %in% colnames(data)) {
    stop("required col 'etr_1' not found")
  }

  if (!"etr_2" %in% colnames(data)) {
    stop("required col 'etr_2' not found")
  }
}

validate_raw_intermediate_csv <- function(data) {
  if (is.null(data)) {
    stop("data is null")
  }

  if (!data.table::is.data.table(data)) {
    stop("data is not a valid data.table")
  }

  if (!"par" %in% colnames(data)) {
    stop("required col 'par' not found")
  }

  if (!"yield_1" %in% colnames(data)) {
    stop("required col 'yield_1' not found")
  }

  if (!"yield_2" %in% colnames(data)) {
    stop("required col 'yield_2' not found")
  }
}

validate_dual_pam_data <- function(data) {
  if (is.null(data)) {
    stop("data is null")
  }

  if (!data.table::is.data.table(data)) {
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

  if (!"Pm.-Det." %in% data[["Action"]]) {
    stop("required value 'Pm' not found in column 'Action'")
  }

  if (!"Fm-Det." %in% data[["Action"]]) {
    stop("required value 'Fm' not found in column 'Action'")
  }
}

validate_junior_pam_data <- function(data) {
  if (is.null(data)) {
    stop("data is null")
  }

  if (!data.table::is.data.table(data)) {
    stop("data is not a valid data.table")
  }

  if (nrow(data) < 2) {
    stop("no data rows")
  }

  if (ncol(data) == 0) {
    stop("no cols in data")
  }

  if (!any(grepl("PAR", colnames(data)))) {
    stop("required col 'PAR' not found")
  }

  if (!any(grepl("Y..II.", colnames(data)))) {
    stop("required col 'Y..II.' not found")
  }

  if (!"Datetime" %in% colnames(data)) {
    stop("required col 'Datetime' not found")
  }
}

validate_etr_regression_data <- function(regression_data) {
  if (is.null(regression_data)) {
    stop("is null")
  }

  if (!data.table::is.data.table(regression_data)) {
    stop("not a valid data.table")
  }

  if (nrow(regression_data) < 2) {
    stop("no regression_data rows")
  }

  if (ncol(regression_data) != 2) {
    stop("regression data got more or less then two columns")
  }

  if (!"par" %in% colnames(regression_data)) {
    stop("required col 'par' not found")
  }

  if (!"prediction" %in% colnames(regression_data)) {
    stop("required col 'prediction' not found")
  }
}

validate_etr_type <- function(etr_type) {
  if (etr_type != etr_1_type && etr_type != etr_2_type) {
    stop("etr type is not valid")
  }
}

validate_model_result <- function(model_result) {
  if (is.list(model_result) == FALSE) {
    stop("model result is not a valid list")
  }

  etr_type <- model_result[["etr_type"]]
  validate_etr_type(etr_type)

  etr_regression_data <- model_result[["etr_regression_data"]]
  validate_etr_regression_data(etr_regression_data)

  residual_sum_of_squares <- model_result[["residual_sum_of_squares"]]
  if (!is.numeric(residual_sum_of_squares)) {
    stop("residual_sum_of_squares is not a valid number")
  }

  if (is.na(residual_sum_of_squares)) {
    stop("residual_sum_of_squares is NA")
  }
}

validate_modified_model_result <- function(model_result) {
  tryCatch(
    {
      validate_model_result(model_result)

      if (is.null(model_result[["residual_sum_of_squares"]]) ||
        !is.numeric(model_result[["residual_sum_of_squares"]])) {
        stop("residual_sum_of_squares is null or not a valid number")
      }

      if (is.null(model_result[["a"]]) ||
        !is.numeric(model_result[["a"]])) {
        stop("a is null or not a valid number")
      }

      if (is.null(model_result[["b"]]) ||
        !is.numeric(model_result[["b"]])) {
        stop("b is null or not a valid number")
      }

      if (is.null(model_result[["c"]]) ||
        !is.numeric(model_result[["c"]])) {
        stop("c is null or not a valid number")
      }

      if (is.null(model_result[["d"]]) ||
        !is.numeric(model_result[["d"]])) {
        stop("d is null or not a valid number")
      }

      if (is.null(model_result[["alpha"]]) ||
        !is.numeric(model_result[["alpha"]])) {
        stop("alpha is null or not a valid number")
      }
      if (is.null(model_result[["beta"]]) ||
        !is.numeric(model_result[["beta"]])) {
        stop("beta is null or not a valid number")
      }

      if (is.null(model_result[["etrmax_with_photoinhibition"]]) ||
        !is.numeric(model_result[["etrmax_with_photoinhibition"]])) {
        stop("etrmax_with_photoinhibition is null or not a valid number")
      }

      if (is.null(model_result[["etrmax_without_photoinhibition"]]) ||
        !is.numeric(model_result[["etrmax_without_photoinhibition"]])) {
        stop("etrmax_without_photoinhibition is null or not a valid number")
      }

      if (is.null(model_result[["ik_with_photoinhibition"]]) ||
        !is.numeric(model_result[["ik_with_photoinhibition"]])) {
        stop("ik_with_photoinhibition is null or not a valid number")
      }

      if (is.null(model_result[["ik_without_photoinhibition"]]) ||
        !is.numeric(model_result[["ik_without_photoinhibition"]])) {
        stop("ik_without_photoinhibition is null or not a valid number")
      }
      if (is.null(model_result[["etrmax_without_photoinhibition"]]) ||
        !is.numeric(model_result[["etrmax_without_photoinhibition"]])) {
        stop("etrmax_without_photoinhibition is null or not a valid number")
      }
      if (is.null(model_result[["im_with_photoinhibition"]]) ||
        !is.numeric(model_result[["im_with_photoinhibition"]])) {
        stop("im_with_photoinhibition is null or not a valid number")
      }
      if (is.null(model_result[["w"]]) ||
        !is.numeric(model_result[["w"]])) {
        stop("w is null or not a valid number")
      }

      if (is.null(model_result[["ib"]]) ||
        !is.numeric(model_result[["ib"]])) {
        stop("ib is null or not a valid number")
      }

      if (is.null(model_result[["etrmax_with_without_ratio"]]) ||
        !is.numeric(model_result[["etrmax_with_without_ratio"]])) {
        stop("etrmax_with_without_ratio is null or not a valid number")
      }
    },
    error = function(e) {
      stop("not a valid modified model result. error: ", e)
    }
  )
}
