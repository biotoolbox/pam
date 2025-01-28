validate_data <- function(data) {
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

validate_model_result <- function(model_result) {
  if (is.list(model_result) == FALSE) {
    stop("model result is not a valid list")
  }

  etr_type <- model_result[["etr_type"]]
  validate_etr_type(etr_type)

  etr_regression_data <- model_result[["etr_regression_data"]]
  validate_etr_regression_data(etr_regression_data)

  sdiff <- model_result[["sdiff"]]
  if (!is.numeric(sdiff)) {
    stop("sdiff is not a valid number")
  }
}

validate_modified_model_result <- function(model_result) {
  tryCatch(
    {
      validate_model_result(model_result)

      if (is.null(model_result[["sdiff"]]) ||
        !is.numeric(model_result[["sdiff"]])) {
        stop("sdiff is null or not a valid number")
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
