generate_regression_vollenweider_ETR_I <- function(data) {
  return(generate_regression_vollenweider_internal(data, etr_I_col_name))
}

generate_regression_vollenweider_ETR_II <- function(data) {
  return(generate_regression_vollenweider_internal(data, etr_II_col_name))
}

generate_regression_vollenweider_internal <- function(data, etr_type) {
  library(data.table)
  library(minpack.lm)
  library(dplyr)

  validate_etr_type(etr_type)
  validate_data(data)

  model <- nlsLM(
    data[[etr_type]] ~
      (a * PAR / sqrt(1 + (b^2) * (PAR^2))) * (1 / sqrt(1 + (c^2) * (PAR^2)))^n,
    data = data,
    start = list(a = 0.3, b = 0.004, c = -0.0001, n = 1000),
    control = nls.lm.control(maxiter = 1000)
  )

  abc <- coef(model)
  a <- abc[["a"]]
  b <- abc[["b"]]
  c <- abc[["c"]]
  n <- abc[["n"]]

  Iopt <- 0
  max_prediction <- 0
  pars <- c()
  predictions <- c()
  for (p in min(data$PAR):max(data$PAR)) {
    pars <- c(pars, p)
    prediction <- (a * p / sqrt(1 + (b^2) * (p^2))) * (1 / sqrt(1 + (c^2) * (p^2)))^n
    predictions <- c(
      predictions,
      prediction
    )

    if (prediction > max_prediction) {
      max_prediction <- prediction
      Iopt <- p
    }
  }

  etr_regression_data <- data.table(pars, predictions)
  etr_regression_data <- setNames(
    etr_regression_data,
    c(PAR_name, prediction_name)
  )

  # Calculate ETRmax
  etr_max <- (a * Iopt / sqrt(1 + (b^2) * (Iopt^2)) * (1 / sqrt(1 + (c^2) * (Iopt^2)))^n)

  # Calculate alpha using the formula
  alpha <- a

  # Calculate Ik using the formula
  Ik <- etr_max / a

  sdiff <- calculate_sdiff(data, etr_regression_data, etr_type)

  return(list(
    etr_regression_data = etr_regression_data,
    sdiff = sdiff,
    a = a,
    b = b,
    c = c,
    n = n,
    etr_max = etr_max,
    alpha = alpha,
    ik = Ik,
    iopt = Iopt
  ))
}

plot_control_vollenweider <- function(data, title, use_etr_I) {
  validate_data(data)
  plot_control_vollenweider(data, "kekfle", "FALSE")
}

plot_control_vollenweider <- function(data, regression_data, title, use_etr_I) {
  validate_data(data)

  if (!is.logical(use_etr_I)) {
    stop("use_etr_I is not a valid bool")
  }

  a <- eval(regression_data[["a"]])
  b <- eval(regression_data[["b"]])
  c <- eval(regression_data[["c"]])
  n <- eval(regression_data[["n"]])
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
    geom_line(data = etr_regression_data, aes(x = PAR, y = prediction), color = "#e70f1f") +
    labs(x = par_label, y = etr_label, title = eval(title)) +
    theme_minimal() +
    labs(caption = glue("ETRmax: {round(regression_data[['etr_max']], 3)}
    alpha: {round(regression_data[['alpha']], 3)}
    Ik: {round(regression_data[['ik']], 3)}
    Iopt: {round(regression_data[['iopt']], 3)}
    SDiff: {round(regression_data[['sdiff']], 3)}")) +
    theme(plot.caption = element_text(hjust = 0))

  return(plot)
}
