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

  # Calculate w using the formula
  w <- b / sqrt(a * c)

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
    im = Im,
    w = w
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
    w: {round(regression_data[['w']], 3)}
    SDiff: {round(regression_data[['sdiff']], 3)}")) +
    theme(plot.caption = element_text(hjust = 0))

  return(plot)
}