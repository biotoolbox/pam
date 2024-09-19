etr_I_col_name <- "recalc_ETR.I."
etr_II_col_name <- "recalc_ETR.II."

PAR_name <- "PAR"
prediction_name <- "prediction"

etr_label <- expression(paste("ETR [", mu, "mol electrons"^{
  -2
} ~ "s"^{
  -1
} ~ "]"))

par_label <- expression(paste("PAR [", mu, "mol photons m"^{
  -2
} ~ "s"^{
  -1
} ~ "]"))

etr_unit_label <- expression(paste("[", mu, "mol electrons"^{
  -2
} ~ "s"^{
  -1
} ~ "]"))

validate_data <- function(data) {
  library(data.table)
  library(glue)

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
    stop(glue("required col 'ID' not found"))
  }

  if (!"PAR" %in% colnames(data)) {
    stop(glue("required col 'PAR' not found"))
  }

  if (!"Y.I." %in% colnames(data) && !"Y.II." %in% colnames(data)) {
    stop(glue("required col 'Y(I)' and 'Y(II)' not found"))
  }

  if (!"Action" %in% colnames(data)) {
    stop(glue("required col 'Action' not found"))
  }

  if (!"Date" %in% colnames(data)) {
    stop(glue("required col 'Date' not found"))
  }

  if (!"Time" %in% colnames(data)) {
    stop(glue("required col 'Time' not found"))
  }
}

read_pam_data <- function(
    csv_path,
    remove_recovery = TRUE,
    etr_factor = 0.84,
    p_ratio = 0.5) {
  library(data.table)
  library(dplyr)

  data <- read.csv(csv_path, sep = ";", dec = ".")
  data <- as.data.table(data)

  validate_data(data)
  data <- data[data$ID == "SP", ]

  last_par <- as.numeric(0)
  date_time_col_values <- c()
  etr_I_col_values <- c()
  etr_II_col_values <- c()

  for (i in seq_len(nrow(data))) {
    row <- data[i, ]
    current_par <- row$PAR

    if (remove_recovery && last_par != 0 && current_par < last_par) {
      data <- data[-(seq_len(nrow(data)) - i), ]
      break
    }

    date_time_row_value <- as.POSIXct(
      paste(row$Date, row$Time, sep = " "),
      tz = "GMT", "%d.%m.%y %H:%M:%S"
    )
    date_time_col_values <- c(date_time_col_values, date_time_row_value)

    yield_I <- row$Y.I.
    recalc_ETRI <- calc_etr(yield_I, current_par, etr_factor, p_ratio)
    etr_I_col_values <- c(etr_I_col_values, recalc_ETRI)

    yield_II <- row$Y.II.
    recalc_ETRII <- calc_etr(yield_II, current_par, etr_factor, p_ratio)
    etr_II_col_values <- c(etr_II_col_values, recalc_ETRII)

    last_par <- current_par
  }




  data <- data %>%
    mutate(!!etr_II_col_name := etr_II_col_values) %>%
    select(!!etr_II_col_name, everything())

  data <- data %>%
    mutate(!!etr_I_col_name := etr_I_col_values) %>%
    select(!!etr_I_col_name, everything())

  data <- data %>%
    mutate(DateTime = date_time_col_values) %>%
    select(DateTime, everything())

  data <- data[order(data$DateTime), ]
  return(data)
}

calc_etr <- function(yield, par, etr_factor, p_ratio) {
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
    etr_to_use <- etr_I_col_name
    data <- data[Action != "Fm-Det."]
  } else {
    etr_to_use <- etr_II_col_name
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
  library(dplyr)

  finalSdiff <- 0
  for (i in seq_len(nrow(data))) {
    row <- data[i, ]
    realEtr <- row[[eval(etr_type)]]

    predictedEtr <- etr_regression_data %>%
      filter(!!PAR_name == row$PAR)

    sdiff <- (predictedEtr - realEtr)^2
    finalSdiff <- finalSdiff + sdiff
  }

  return(finalSdiff)
}

validate_etr_type <- function(etr_type) {
  if (etr_type != etr_I_col_name && etr_type != etr_II_col_name) {
    stop("etr type is not valid")
  }
}
