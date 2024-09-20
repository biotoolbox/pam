generate_regression_platt_ETR_I <- function(data) {
  return(generate_regression_platt_internal(data, etr_I_col_name))
}

generate_regression_platt_ETR_II <- function(data) {
  return(generate_regression_platt_internal(data, etr_II_col_name))
}

generate_regression_platt_internal <- function(data, etr_type) {
  library(minpack.lm)
  library(SciViews)
  library(data.table)

  validate_data(data)
  validate_etr_type(etr_type)

  data <- remove_det_row_by_etr(data, etr_type)

  model <- nlsLM(data[[etr_type]] ~ ETRmPot * (1 - exp((-alpha * PAR) / ETRmPot)) * exp((-beta * PAR) / ETRmPot),
    data = data,
    start = list(alpha = 0.3, beta = 0.01, ETRmPot = 30)
  )

  abc <- coef(model)
  alpha <- abc[["alpha"]]
  beta <- abc[["beta"]]
  ETRmPot <- abc[["ETRmPot"]]

  # Calculate ETRmax using the formula
  ETRmax <- ETRmPot * (alpha / (alpha + beta)) * ((beta / (alpha + beta))^(beta / alpha))

  # Calculate Ik using the formula
  ik <- ETRmax / alpha

  pars <- c()
  predictions <- c()
  for (p in min(data$PAR):max(data$PAR)) {
    pars <- c(pars, p)
    predictions <- c(predictions, ETRmPot * (1 - exp((-alpha * p) / ETRmPot)) * exp((-beta * p) / ETRmPot))
  }
  etr_regression_data <- data.table("PAR" = pars, "prediction" = predictions)

  sdiff <- calculate_sdiff(data, etr_regression_data, etr_type)

  return(list(
    etr_regression_data = etr_regression_data,
    sdiff = sdiff,
    alpha = alpha,
    beta = beta,
    ETRmPot = ETRmPot,
    etr_max = ETRmax,
    ik = ik
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
  a <- eval(regression_data[["alpha"]])
  b <- eval(regression_data[["beta"]])
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
