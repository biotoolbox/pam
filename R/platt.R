alpha_start_value_platt_default <- 0.3
beta_start_value_platt_default <- 0.01
etrmpot_start_value_platt_default <- 30

generate_regression_platt_ETR_I <- function(
    data,
    alpha_start_value = alpha_start_value_platt_default,
    beta_start_value = beta_start_value_platt_default,
    etrmpo_start_value = etrmpot_start_value_platt_default) {
  return(generate_regression_platt_internal(
    data,
    etr_I_col_name,
    alpha_start_value,
    beta_start_value,
    etrmpo_start_value
  ))
}

generate_regression_platt_ETR_II <- function(
    data,
    alpha_start_value = alpha_start_value_platt_default,
    beta_start_value = beta_start_value_platt_default,
    etrmpot_start_value = etrmpot_start_value_platt_default) {
  return(generate_regression_platt_internal(
    data,
    etr_II_col_name,
    alpha_start_value,
    beta_start_value,
    etrmpot_start_value
  ))
}

generate_regression_platt_internal <- function(
    data,
    etr_type,
    alpha_start_value = alpha_start_value_platt_default,
    beta_start_value = beta_start_value_platt_default,
    etrmpot_start_value = etrmpot_start_value_platt_default) {
  library(minpack.lm)
  library(SciViews)
  library(data.table)

  tryCatch(
    {
      validate_data(data)
      validate_etr_type(etr_type)

      if (!is.numeric(alpha_start_value)) {
        stop("alpha start value is not a valid number")
      }
      if (!is.numeric(beta_start_value)) {
        stop("beta start value is not a valid number")
      }
      if (!is.numeric(etrmpot_start_value)) {
        stop("etrmpot start value is not a valid number")
      }

      data <- remove_det_row_by_etr(data, etr_type)

      model <- nlsLM(data[[etr_type]] ~ etrmpot * (1 - exp((-alpha * PAR) / etrmpot)) * exp((-beta * PAR) / etrmpot),
        data = data,
        start = list(
          alpha = alpha_start_value,
          beta = beta_start_value,
          etrmpot = etrmpot_start_value
        ),
        control = nls.control(maxiter = 1000)
      )

      abc <- coef(model)
      alpha <- abc[["alpha"]]
      beta <- abc[["beta"]]
      etrmpot <- abc[["etrmpot"]]

      # Calculate ETRmax using the formula
      ETRmax <- etrmpot * (alpha / (alpha + beta)) * ((beta / (alpha + beta))^(beta / alpha))

      # Calculate Ik using the formula
      ik <- ETRmax / alpha

      pars <- c()
      predictions <- c()
      for (p in min(data$PAR):max(data$PAR)) {
        pars <- c(pars, p)
        predictions <- c(predictions, etrmpot * (1 - exp((-alpha * p) / etrmpot)) * exp((-beta * p) / etrmpot))
      }
      etr_regression_data <- data.table(
        "PAR" = pars,
        "prediction" = predictions
      )

      sdiff <- calculate_sdiff(data, etr_regression_data, etr_type)

      return(list(
        etr_regression_data = etr_regression_data,
        sdiff = sdiff,
        alpha = alpha,
        beta = beta,
        etrmpot = etrmpot,
        etr_max = ETRmax,
        ik = ik
      ))
    },
    warning = function(w) {
      stop("Warning while calculating platt model: ", w)
    },
    error = function(e) {
      stop("Error while calculating platt model: ", e)
    }
  )
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
