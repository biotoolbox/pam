etr_max_start_value_walsby_default <- 100
alpha_start_value_walsby_default <- 0.4
beta_start_value_walsby_default <- -0.01

generate_regression_walsby_ETR_I <- function(
    data,
    etr_max_start_value = etr_max_start_value_walsby_default,
    alpha_start_value = alpha_start_value_walsby_default,
    beta_start_value = alpha_start_value_walsby_default) {
  return(
    generate_regression_walsby_internal(
      data,
      etr_I_type,
      etr_max_start_value,
      alpha_start_value,
      beta_start_value
    )
  )
}

generate_regression_walsby_ETR_II <- function(
    data,
    etr_max_start_value = etr_max_start_value_walsby_default,
    alpha_start_value = alpha_start_value_walsby_default,
    beta_start_value = beta_start_value_walsby_default) {
  return(generate_regression_walsby_internal(
    data,
    etr_II_type,
    etr_max_start_value,
    alpha_start_value,
    beta_start_value
  ))
}

generate_regression_walsby_internal <- function(
    data,
    etr_type,
    etr_max_start_value = etr_max_start_value_walsby_default,
    alpha_start_value = alpha_start_value_walsby_default,
    beta_start_value = beta_start_value_walsby_default) {
  library(data.table)
  library(minpack.lm)
  library(SciViews)

  tryCatch(
    {
      validate_data(data)
      validate_etr_type(etr_type)

      if (!is.numeric(etr_max_start_value)) {
        stop("etr max start value is not a valid number")
      }
      if (!is.numeric(alpha_start_value)) {
        stop("alpha start value is not a valid number")
      }
      if (!is.numeric(beta_start_value)) {
        stop("beta start value is not a valid number")
      }

      data <- remove_det_row_by_etr(data, etr_type)

      model <- nlsLM(data[[etr_type]] ~ etr_max * (1 - exp((-alpha * PAR) / etr_max)) + beta * PAR,
        data = data,
        start = list(
          etr_max = etr_max_start_value,
          alpha = alpha_start_value,
          beta = beta_start_value
        ),
        control = nls.control(maxiter = 1000)
      )

      abc <- coef(model)
      etr_max <- abc[["etr_max"]]
      alpha <- abc[["alpha"]]
      beta <- abc[["beta"]]

      pars <- c()
      predictions <- c()
      for (p in min(data$PAR):max(data$PAR)) {
        pars <- c(pars, p)
        predictions <- c(predictions, etr_max * (1 - exp((-alpha * p) / etr_max)) + beta * p)
      }

      etr_regression_data <- data.table(
        "PAR" = pars,
        "prediction" = predictions
      )

      ik <- NA_real_
      tryCatch(
        {
          ik <- etr_max / alpha
        },
        warning = function(w) {
          message("failed to calculate ik: Warning:", w)
        },
        error = function(e) {
          message("failed to calculate ik: Error:", e)
        }
      )

      
      sdiff <- NA_real_
      tryCatch(
        {
        sdiff <- calculate_sdiff(data, etr_regression_data, etr_type)
        },
        warning = function(w) {
          message("failed to calculate sdiff: Warning:", w)
        },
        error = function(e) {
          message("failed to calculate  sdiff: Error:", e)
        }
      )

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

plot_control_walsby <- function(data, regression_data, title, etr_type) {
  library(ggplot2)
  library(glue)
  library(ggthemes)
  library(gridExtra)

  validate_data(data)
  validate_etr_type(etr_type)

  etr_regression_data <- eval(regression_data[["etr_regression_data"]])

  data <- remove_det_row_by_etr(data, etr_type)

  values <- c(
    as.character(round(regression_data[["etr_max"]], 3)),
    as.character(round(regression_data[["alpha"]], 3)),
    as.character(round(regression_data[["ik"]], 3)),
    as.character(round(regression_data[["beta"]], 3)),
    as.character(round(regression_data[["sdiff"]], 3))
  )

  params <- data.frame(
    Parameter = c("ETRmax", "alpha", "Ik", "beta", "SDiff"),
    Value = unlist(values)
  )

  params_transposed <- t(params)
  colnames(params_transposed) <- NULL
  rownames(params_transposed) <- NULL

  plot <- ggplot(data, aes(x = PAR, y = get(etr_type))) +
    geom_point() +
    geom_line(data = etr_regression_data, aes(x = PAR, y = prediction), color = "#f700ff") +
    labs(x = par_label, y = etr_label, title = eval(title)) +
    theme_base()

  table <- tableGrob(params_transposed, rows = NULL, theme = ttheme_minimal())
  full_plot <- grid.arrange(plot, table, ncol = 1, heights = c(3, 0.2), widths = 1.5)
  return(full_plot)
}
