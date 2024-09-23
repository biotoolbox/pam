a_start_value_vollenweider_default <- 0.3
b_start_value_vollenweider_default <- 0.004
c_start_value_vollenweider_default <- -0.0001
n_start_value_vollenweider_default <- 1000

generate_regression_vollenweider_ETR_I <- function(
    data,
    a_start_value = a_start_value_vollenweider_default,
    b_start_value = b_start_value_vollenweider_default,
    c_start_value = c_start_value_vollenweider_default,
    n_start_value = n_start_value_vollenweider_default) {
  return(generate_regression_vollenweider_internal(
    data,
    etr_I_type,
    a_start_value,
    b_start_value,
    c_start_value,
    n_start_value
  ))
}

generate_regression_vollenweider_ETR_II <- function(
    data,
    a_start_value = a_start_value_vollenweider_default,
    b_start_value = b_start_value_vollenweider_default,
    c_start_value = c_start_value_vollenweider_default,
    n_start_value = n_start_value_vollenweider_default) {
  return(generate_regression_vollenweider_internal(
    data,
    etr_II_type,
    a_start_value,
    b_start_value,
    c_start_value,
    n_start_value
  ))
}

generate_regression_vollenweider_internal <- function(
    data,
    etr_type,
    a_start_value = a_start_value_vollenweider_default,
    b_start_value = b_start_value_vollenweider_default,
    c_start_value = c_start_value_vollenweider_default,
    n_start_value = n_start_value_vollenweider_default) {
  library(data.table)
  library(minpack.lm)

  tryCatch(
    {
      validate_etr_type(etr_type)
      validate_data(data)

      if (!is.numeric(a_start_value)) {
        stop("a start value is not a valid number")
      }
      if (!is.numeric(b_start_value)) {
        stop("b start value is not a valid number")
      }
      if (!is.numeric(c_start_value)) {
        stop("c start value is not a valid number")
      }
      if (!is.numeric(n_start_value)) {
        stop("n start value is not a valid number")
      }

      data <- remove_det_row_by_etr(data, etr_type)

      model <- nlsLM(
        data[[etr_type]] ~
          (a * PAR / sqrt(1 + (b^2) * (PAR^2))) * (1 / sqrt(1 + (c^2) * (PAR^2)))^n,
        data = data,
        start = list(
          a = a_start_value,
          b = b_start_value,
          c = c_start_value,
          n = n_start_value
        ),
        control = nls.control(maxiter = 1000)
      )

      abc <- coef(model)
      a <- abc[["a"]]
      b <- abc[["b"]]
      c <- abc[["c"]]
      n <- abc[["n"]]
      alpha <- a

      iopt <- 0
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
          iopt <- p
        }
      }

      etr_regression_data <- data.table(
        "PAR" = pars,
        "prediction" = predictions
      )

      etr_max <- NA_real_
      tryCatch(
        {
          etr_max <- (a * iopt / sqrt(1 + (b^2) * (iopt^2)) * (1 / sqrt(1 + (c^2) * (iopt^2)))^n)
        },
        warning = function(w) {
          message("failed to calculate etr_max: Warning:", w)
        },
        error = function(e) {
          message("failed to calculate etr_max: Error:", e)
        }
      )

      ik <- NA_real_
      tryCatch(
        {
          ik <- etr_max / a
        },
        warning = function(w) {
          message("failed to calculate ik: Warning:", w)
        },
        error = function(e) {
          message("failed to calculate ik: Error:", e)
        }
      )

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
        ik = ik,
        iopt = iopt
      ))
    },
    warning = function(w) {
      stop("Warning while calculating vollenweider model: ", w)
    },
    error = function(e) {
      stop("Error while calculating vollenweider model: ", e)
    }
  )
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
    etr_to_use <- etr_I_type
    data <- data[Action != "Fm-Det."]
  } else {
    etr_to_use <- etr_II_type
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
