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

read_pam_data <- function(
    csv_path,
    remove_recovery = TRUE,
    etr_factor = 0.84,
    p_ratio = 0.5) {
  library(data.table)
  library(dplyr)

  tryCatch(
    {
      data <- read.csv(csv_path, sep = ";", dec = ".")
      data <- as.data.table(data)

      validate_data(data)
      data <- data[data$ID == "SP", ]

      date_time_col_values <- c()
      for (i in seq_len(nrow(data))) {
        row <- data[i, ]

        date_time_row_value <- as.POSIXct(
          paste(row$Date, row$Time, sep = " "),
          tz = "GMT", "%d.%m.%y %H:%M:%S"
        )
        date_time_col_values <- c(date_time_col_values, date_time_row_value)
      }

      data <- data %>%
        mutate(DateTime = date_time_col_values) %>%
        select(DateTime, everything())
      data <- data[order(data$DateTime), ]

      result <- data.table()
      last_par <- as.numeric(0)
      for (i in seq_len(nrow(data))) {
        row <- data[i, ]
        current_par <- row$PAR

        if (remove_recovery && last_par != 0 && current_par < last_par) {
          break
        }

        yield_I <- row$Y.I.
        recalc_ETRI <- calc_etr(yield_I, current_par, etr_factor, p_ratio)
        row <- cbind(row, etr_I_col_name = recalc_ETRI)
        setnames(row, old = "etr_I_col_name", new = etr_I_type)

        yield_II <- row$Y.II.
        recalc_ETRII <- calc_etr(yield_II, current_par, etr_factor, p_ratio)
        row <- cbind(row, etr_II_col_name = recalc_ETRII)
        setnames(row, old = "etr_II_col_name", new = etr_II_type)

        result <- rbind(result, row)

        last_par <- current_par
      }

      result <- result %>%
        select(!!etr_II_type, everything())

      result <- result %>%
        select(!!etr_I_type, everything())

      result <- result %>%
        select(DateTime, everything())

      return(result)
    },
    warning = function(w) {
      stop("Warning in file: ", csv_path, " Warning: ", w)
    },
    error = function(e) {
      stop("Error in file: ", csv_path, " Error: ", e)
    }
  )
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

plot_control_raw <- function(data, title, use_etr_I) {
  library(ggplot2)
  library(cowplot)

  validate_data(data)

  if (!is.logical(use_etr_I)) {
    stop("use_etr_I is not a valid bool")
  }

  etr_to_use <- ""
  if (use_etr_I) {
    etr_to_use <- etr_I_type
    data <- data[Action != "Fm-Det."]
  } else {
    etr_to_use <- etr_II_type
    data <- data[Action != "Pm.-Det."]
  }

  # Create plot for ETR.II. by PAR and filename
  plot <- ggplot(data, aes(x = PAR, y = get(etr_to_use))) +
    geom_point() +
    labs(x = par_label, y = etr_label, title = eval(title)) +
    theme_minimal() # Adjust theme if needed

  # Print the plot grid to the PDF
  return(plot)
}

calculate_sdiff <- function(data, etr_regression_data, etr_type) {
  finalSdiff <- 0
  for (i in seq_len(nrow(data))) {
    row <- data[i, ]
    realEtr <- row[[etr_type]]
    predictedEtr <- etr_regression_data[etr_regression_data$PAR == row$PAR, ][[prediction_name]]

    sdiff <- (predictedEtr - realEtr)^2
    finalSdiff <- finalSdiff + sdiff
  }

  return(finalSdiff)
}

validate_etr_type <- function(etr_type) {
  if (etr_type != etr_I_type && etr_type != etr_II_type) {
    stop("etr type is not valid")
  }
}

remove_det_row_by_etr <- function(data, etr_type) {
  validate_data(data)

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
