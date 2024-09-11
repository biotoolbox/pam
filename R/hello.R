etr_I_col_name <- "recalc_ETR.I."
etr_II_col_name <- "recalc_ETR.II."

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
  library(glue)

  data <- read.csv(csv_path, sep = ";", dec = ".")
  data <- as.data.table(data)
  validate_data(data)

  data <- data[ID == "SP"]
  data <- data[, DateTime := as.POSIXct(paste(Date, Time, sep = " "), tz = "GMT", "%d.%m.%y %H:%M:%S")]
  data <- data[order(DateTime)]

  result <- data.table()
  last_par <- as.numeric(0)
  for (i in seq_len(nrow(data))) {
    row <- data[i, ]
    current_par <- row$PAR
    if (!is.numeric(current_par)) {
      stop(glue("PAR in row {i} is not numeric"))
    }

    yield_I <- row$Y.I.
    recalc_ETRI <- calc_etr(yield_I, current_par, etr_factor, p_ratio)
    row[, (etr_I_col_name) := recalc_ETRI]

    yield_II <- row$Y.II.
    recalc_ETRII <- calc_etr(yield_II, current_par, etr_factor, p_ratio)
    row[, (etr_II_col_name) := recalc_ETRII]

    if (remove_recovery && last_par != 0 && current_par < last_par) {
      data <- data[!i, ]
      break
    }
    last_par <- current_par
    result <- rbind(result, row)
  }

  return(result)
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

generate_regression_eilers_peeters <- function(data, use_etr_I) {
  library(minpack.lm)
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

  model <- nls(data[[etr_to_use]] ~ (PAR / (a * PAR^2 + b * PAR + c)),
    data = data,
    start = list(a = 0.00004, b = 0.004, c = 5)
  ) # Initial parameter values

  abc <- coef(model)
  a <- abc[["a"]]
  b <- abc[["b"]]
  c <- abc[["c"]]
  etr_max <- 1 / (b + 2 * sqrt(a * c))

  # Calculate alpha using the formula
  alpha <- 1 / c

  # Calculate Ik using the formula
  Ik <- c / (b + 2 * sqrt(a * c))

  # Calculate Im using the formula
  Im <- sqrt(c / a)


  return(c(a = a, b = b, c = c, etr_max = etr_max, alpha = alpha, ik = Ik, im = Im))
}

plot_control_eilers_peeters <- function(data, regression_data, title, use_etr_I) {
  library(ggplot2)
  library(glue)

  validate_data(data)

  if (!is.logical(use_etr_I)) {
    stop("use_etr_I is not a valid bool")
  }

  a <- eval(regression_data[["a"]])
  b <- eval(regression_data[["b"]])
  c <- eval(regression_data[["c"]])

  pars <- c()
  predictions <- c()
  for (p in min(data$PAR):max(data$PAR)) {
    pars <- c(pars, p)
    predictions <- c(predictions, p / ((a * p^2) + (b * p) + c))
  }
  line_data <- data.table("PAR" = pars, "prediction" = predictions)

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
    geom_line(data = line_data, aes(x = PAR, y = prediction), color = "#f700ff") +
    labs(x = par_label, y = etr_label, title = eval(title)) +
    theme_minimal() +
    labs(caption = glue("ETRmax: {round(regression_data[['etr_max']], 3)}
    alpha: {round(regression_data[['alpha']], 3)}
    Ik: {round(regression_data[['ik']], 3)}
    Im: {round(regression_data[['im']], 3)}")) +
    theme(plot.caption = element_text(hjust = 0))

  return(plot)
}
