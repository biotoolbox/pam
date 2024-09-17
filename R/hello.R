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

calculate_sdiff <- function(data, etr_regression_data, etr_to_use) {
  finalSdiff <- 0
  for (i in seq_len(nrow(data))) {
    row <- data[i]
    realEtr <- row[[etr_to_use]]
    predictedEtr <- etr_regression_data[PAR == row$PAR, prediction]
    sdiff <- (predictedEtr - realEtr)^2
    finalSdiff <- finalSdiff + sdiff
  }

  return(finalSdiff)
}

####################

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

  model <- nlsLM(data[[etr_to_use]] ~ (PAR / (a * PAR^2 + b * PAR + c)),
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

  pars <- c()
  predictions <- c()
  for (p in min(data$PAR):max(data$PAR)) {
    pars <- c(pars, p)
    predictions <- c(predictions, p / ((a * p^2) + (b * p) + c))
  }
  etr_regression_data <- data.table("PAR" = pars, "prediction" = predictions)

  return(list(
    etr_regression_data = etr_regression_data,
    sdiff = calculate_sdiff(data, etr_regression_data, etr_to_use),
    a = a,
    b = b,
    c = c,
    etr_max = etr_max,
    alpha = alpha,
    ik = Ik,
    im = Im
  ))
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
  etr_regression_data <- eval(regression_data[["etr_regression_data"]])

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
    geom_line(data = etr_regression_data, aes(x = PAR, y = prediction), color = "#f700ff") +
    labs(x = par_label, y = etr_label, title = eval(title)) +
    theme_minimal() +
    labs(caption = glue("ETRmax: {round(regression_data[['etr_max']], 3)}
    alpha: {round(regression_data[['alpha']], 3)}
    Ik: {round(regression_data[['ik']], 3)}
    Im: {round(regression_data[['im']], 3)}
    SDiff: {round(regression_data[['sdiff']], 3)}")) +
    theme(plot.caption = element_text(hjust = 0))

  return(plot)
}

generate_regression_walsby <- function(data, use_etr_I) {
  library(minpack.lm)
  library(SciViews)
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

  model <- nlsLM(data[[etr_to_use]] ~ a * (1 - exp((-b * PAR) / a)) + c * PAR,
    data = data,
    start = list(a = 100, b = 0.4, c = -0.01)
  )

  abc <- coef(model)
  a <- abc[["a"]]
  b <- abc[["b"]]
  c <- abc[["c"]]

  # Calculate Iopt using the formula
  Iopt <- -(a * ln(-c / b)) / b

  # Calculate ETRmax using the formula
  ETRmax <- a * (1 - exp(-b / a * Iopt)) + c * Iopt

  # Calculate alpha using the formula
  alpha <- b

  # Calculate Ik using the formula
  Ik <- a / b

  # Calculate beta using the formula
  beta <- c

  pars <- c()
  predictions <- c()
  for (p in min(data$PAR):max(data$PAR)) {
    pars <- c(pars, p)
    predictions <- c(predictions, a * (1 - exp((-b * p) / a)) + c * p)
  }
  etr_regression_data <- data.table("PAR" = pars, "prediction" = predictions)

  return(list(
    etr_regression_data = etr_regression_data,
    sdiff = calculate_sdiff(data, etr_regression_data, etr_to_use),
    a = a,
    b = b,
    c = c,
    iopt = Iopt,
    etr_max = ETRmax,
    alpha = alpha,
    ik = Ik,
    beta = beta
  ))
}

plot_control_walsby <- function(data, regression_data, title, use_etr_I) {
  library(ggplot2)
  library(glue)

  validate_data(data)

  if (!is.logical(use_etr_I)) {
    stop("use_etr_I is not a valid bool")
  }

  a <- eval(regression_data[["a"]])
  b <- eval(regression_data[["b"]])
  c <- eval(regression_data[["c"]])
  etr_regression_data <- regression_data[["etr_regression_data"]]

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
    geom_line(data = etr_regression_data, aes(x = PAR, y = prediction), color = "#f700ff") +
    labs(x = par_label, y = etr_label, title = eval(title)) +
    theme_minimal() +
    labs(caption = glue("ETRmax: {round(regression_data[['etr_max']], 3)}
    alpha: {round(regression_data[['alpha']], 3)}
    Ik: {round(regression_data[['ik']], 3)}
    beta: {round(regression_data[['beta']], 3)}
    Iopt: {round(regression_data[['iopt']], 3)}
    SDiff: {round(regression_data[['sdiff']], 3)}")) +
    theme(plot.caption = element_text(hjust = 0))

  return(plot)
}

generate_regression_platt <- function(data, use_etr_I) {
  library(minpack.lm)
  library(SciViews)
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

  model <- nlsLM(data[[etr_to_use]] ~ ETRmPot * (1 - exp((-a * PAR) / ETRmPot)) * exp((-b * PAR) / ETRmPot),
    data = data,
    start = list(a = 0.3, b = 0.01, ETRmPot = 30)
  )

  abc <- coef(model)
  a <- abc[["a"]]
  b <- abc[["b"]]
  ETRmPot <- abc[["ETRmPot"]]

  # Calculate ETRmax using the formula
  ETRmax <- ETRmPot * (a / (a + b)) * ((b / (a + b))^(b / a))

  # Calculate alpha using the formula
  alpha <- a

  # Calculate Ik using the formula
  Ik <- ETRmax / a

  # Calculate beta using the formula
  beta <- b

  pars <- c()
  predictions <- c()
  for (p in min(data$PAR):max(data$PAR)) {
    pars <- c(pars, p)
    predictions <- c(predictions, ETRmPot * (1 - exp((-a * p) / ETRmPot)) * exp((-b * p) / ETRmPot))
  }
  etr_regression_data <- data.table("PAR" = pars, "prediction" = predictions)

  return(list(
    sdiff = calculate_sdiff(data, etr_regression_data, etr_to_use),
    etr_regression_data = etr_regression_data,
    a = a,
    b = b,
    ETRmPot = ETRmPot,
    etr_max = ETRmax,
    alpha = alpha,
    ik = Ik,
    beta = beta
  ))
}

plot_control_platt <- function(data, regression_data, title, use_etr_I) {
  library(ggplot2)
  library(glue)

  validate_data(data)
  # TODO validate regression data

  if (!is.logical(use_etr_I)) {
    stop("use_etr_I is not a valid bool")
  }

  sdiff <- eval(regression_data[["sdiff"]])
  etr_regression_data <- eval(regression_data[["etr_regression_data"]])
  a <- eval(regression_data[["a"]])
  b <- eval(regression_data[["b"]])
  ETRmPot <- eval(regression_data[["ETRmPot"]])

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
    geom_line(data = etr_regression_data, aes(x = PAR, y = prediction), color = "#f700ff") +
    labs(x = par_label, y = etr_label, title = eval(title)) +
    theme_minimal() +
    labs(caption = glue("ETRmax: {round(regression_data[['etr_max']], 3)}
    alpha: {round(regression_data[['alpha']], 3)}
    Ik: {round(regression_data[['ik']], 3)}
    beta: {round(regression_data[['beta']], 3)}
    SDiff: {round(regression_data[['sdiff']], 3)}")) +
    theme(plot.caption = element_text(hjust = 0))

  return(plot)
}