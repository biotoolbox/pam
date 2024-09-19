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