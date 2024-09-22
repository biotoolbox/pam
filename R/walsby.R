generate_regression_walsby_ETR_I <- function(data) {
  return(generate_regression_walsby_internal(data, etr_I_col_name))
}

generate_regression_walsby_ETR_II <- function(data) {
  return(generate_regression_walsby_internal(data, etr_II_col_name))
}

generate_regression_walsby_internal <- function(data, etr_type) {
  library(data.table)
  library(minpack.lm)
  library(SciViews)

  tryCatch(
    {
      validate_data(data)

      data <- remove_det_row_by_etr(data, etr_type)

      model <- nlsLM(data[[etr_type]] ~ etr_max * (1 - exp((-alpha * PAR) / etr_max)) + beta * PAR,
        data = data,
        start = list(etr_max = 100, alpha = 0.4, beta = -0.01),
        control = nls.control(maxiter = 1000)
      )

      abc <- coef(model)
      etr_max <- abc[["etr_max"]]
      alpha <- abc[["alpha"]]
      beta <- abc[["beta"]]

      # Calculate ik using the formula
      ik <- etr_max / alpha

      pars <- c()
      predictions <- c()
      for (p in min(data$PAR):max(data$PAR)) {
        pars <- c(pars, p)
        predictions <- c(predictions, etr_max * (1 - exp((-alpha * p) / etr_max)) + beta * p)
      }

      etr_regression_data <- data.table(pars, predictions)
      etr_regression_data <- setNames(
        etr_regression_data,
        c(PAR_name, prediction_name)
      )

      sdiff <- calculate_sdiff(data, etr_regression_data, etr_type)

      return(list(
        etr_regression_data = etr_regression_data,
        sdiff = sdiff,
        etr_max = etr_max,
        alpha = alpha,
        ik = ik,
        beta = beta
      ))
    },
    warning = function(w) {
      stop("Warning while calculating walsby model: ", w)
    },
    error = function(e) {
      stop("Error while calculating walsby model: ", e)
    }
  )
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
