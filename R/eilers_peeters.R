a_start_values_eilers_peeters_default <- 0.00004
b_start_values_eilers_peeters_default <- 0.004
c_start_values_eilers_peeters_default <- 5

generate_regression_eilers_peeters_ETR_I <- function(
    data,
    a_start_value = a_start_values_eilers_peeters_default,
    b_start_value = b_start_values_eilers_peeters_default,
    c_start_value = c_start_values_eilers_peeters_default) {
  return(generate_regression_eilers_peeters_internal(
    data,
    etr_I_type,
    a_start_value,
    b_start_value,
    c_start_value
  ))
}

generate_regression_eilers_peeters_ETR_II <- function(
    data,
    a_start_value = a_start_values_eilers_peeters_default,
    b_start_value = b_start_values_eilers_peeters_default,
    c_start_value = c_start_values_eilers_peeters_default) {
  return(generate_regression_eilers_peeters_internal(
    data,
    etr_II_type,
    a_start_value,
    b_start_value,
    c_start_value
  ))
}

generate_regression_eilers_peeters_internal <- function(
    data,
    etr_type,
    a_start_value = a_start_values_eilers_peeters_default,
    b_start_value = b_start_values_eilers_peeters_default,
    c_start_value = c_start_values_eilers_peeters_default) {
  library(minpack.lm)
  library(data.table)

  tryCatch(
    {
      validate_data(data)
      validate_etr_type(etr_type)

      if (!is.numeric(a_start_value)) {
        stop("a start value is not a valid number")
      }
      if (!is.numeric(b_start_value)) {
        stop("b start value is not a valid number")
      }
      if (!is.numeric(c_start_value)) {
        stop("c start value is not a valid number")
      }

      data <- remove_det_row_by_etr(data, etr_type)
      model <- nlsLM(data[[etr_type]] ~ (PAR / ((a * PAR^2) + (b * PAR) + c)),
        data = data,
        start = list(a = a_start_value, b = b_start_value, c = c_start_value),
        control = nls.lm.control(maxiter = 1000)
      )

      abc <- coef(model)
      a <- abc[["a"]]
      b <- abc[["b"]]
      c <- abc[["c"]]

      etr_max <- NA_real_
      tryCatch(
        {
          etr_max <- 1 / (b + 2 * sqrt(a * c))
        },
        warning = function(w) {
          message("failed to calculate etr_max: Warning:", w)
        },
        error = function(e) {
          message("failed to calculate etr_max: Error:", e)
        }
      )

      alpha <- NA_real_
      tryCatch(
        {
          alpha <- 1 / c
        },
        warning = function(w) {
          message("failed to calculate alpha: Warning:", w)
        },
        error = function(e) {
          message("failed to calculate alpha: Error:", e)
        }
      )

      ik <- NA_real_
      tryCatch(
        {
          ik <- c / (b + 2 * sqrt(a * c))
        },
        warning = function(w) {
          message("failed to calculate Ik: Warning:", w)
        },
        error = function(e) {
          message("failed to calculate Ik: Error:", e)
        }
      )

      im <- NA_real_
      tryCatch(
        {
          im <- sqrt(c / a)
        },
        warning = function(w) {
          message("failed to calculate Im: Warning:", w)
        },
        error = function(e) {
          message("failed to calculate Im: Error:", e)
        }
      )
      w <- NA_real_
      tryCatch(
        {
          w <- b / sqrt(a * c)
        },
        warning = function(w) {
          message("failed to calculate w: Warning:", w)
        },
        error = function(e) {
          message("failed to calculate w: Error:", e)
        }
      )

      pars <- c()
      predictions <- c()
      for (p in min(data$PAR):max(data$PAR)) {
        pars <- c(pars, p)
        predictions <- c(predictions, p / ((a * p^2) + (b * p) + c))
      }
      etr_regression_data <- data.table(
        "PAR" = pars,
        "prediction" = predictions
      )

      sdiff <- calculate_sdiff(data, etr_regression_data, etr_type)

      return(list(
        etr_regression_data = etr_regression_data,
        sdiff = sdiff,
        a = a,
        b = b,
        c = c,
        etr_max = etr_max,
        alpha = alpha,
        ik = ik,
        im = im,
        w = w
      ))
    },
    warning = function(w) {
      stop("Warning while calculating eilers peeters model: ", w)
    },
    error = function(e) {
      stop("Error while calculating eilers peeters model: ", e)
    }
  )
}

plot_control_eilers_peeters <- function(data, regression_data, title, etr_type) {
  library(ggplot2)
  library(glue)
  library(ggthemes)
  library(gridExtra)

  validate_data(data)
  validate_etr_type(etr_type)

  a <- eval(regression_data[["a"]])
  b <- eval(regression_data[["b"]])
  c <- eval(regression_data[["c"]])

  etr_regression_data <- eval(regression_data[["etr_regression_data"]])

  data <- remove_det_row_by_etr(data, etr_type)

  values <- c(
    as.character(round(a, 7)),
    as.character(round(b, 6)),
    as.character(round(c, 6)),
    as.character(round(regression_data[["etr_max"]], 3)),
    as.character(round(regression_data[["alpha"]], 3)),
    as.character(round(regression_data[["ik"]], 3)),
    as.character(round(regression_data[["im"]], 3)),
    as.character(round(regression_data[["w"]], 3)),
    as.character(round(regression_data[["sdiff"]], 3))
  )

  params <- data.frame(
    Parameter = c("a", "b", "c", "ETRmax", "alpha", "Ik", "Im", "W", "SDiff"),
    Value = unlist(values)
  )

  params_transposed <- t(params)
  colnames(params_transposed) <- NULL
  rownames(params_transposed) <- NULL

  # Create plot for ETR.II. by PAR and filename
  plot <- ggplot(data, aes(x = PAR, y = get(etr_type))) +
    geom_point() +
    geom_line(data = etr_regression_data, aes(x = PAR, y = prediction), color = "#f700ff") +
    labs(x = par_label, y = etr_label, title = eval(title)) +
    theme_base()

  table <- tableGrob(params_transposed, rows = NULL, theme = ttheme_minimal())
  full_plot <- grid.arrange(plot, table, ncol = 1, heights = c(3, 0.2), widths = 1.5)
  return(full_plot)
}
