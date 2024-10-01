#' default a start value for eilers_peeters_regression regression (0.00004)
a_start_values_eilers_peeters_default <- 0.00004
#' default b start value for eilers_peeters_regression regression (0.004)
b_start_values_eilers_peeters_default <- 0.004
#' default c start value for eilers_peeters_regression regression (5)
c_start_values_eilers_peeters_default <- 5

#' generate regression for eilers and peeters and ETR I
#' @param data (required): the raw data from csv file
#' @param a_start_value (optional): the start values used for the regression model @seealso a_start_value_eilers_peeters_default
#' @param b_start_value (optional): the start values used for the regression model @seealso b start value eilers_peeters default
#' @param c_start_value (optional): the start values used for the regression model @seealso c start value eilers_peeters default
#' @return list with regression data table and the calculated values: etrmax, alpha, ik...
#' @export
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

#' generate regression for eilers and peeters and ETR II
#' @param data (required): the raw data from csv file
#' @param a_start_value (optional): the start values used for the regression model @seealso a_start_value_eilers_peeters_default
#' @param b_start_value (optional): the start values used for the regression model @seealso b start value eilers_peeters default
#' @param c_start_value (optional): the start values used for the regression model @seealso c start value eilers_peeters default
#' @return list with regression data table and the calculated values: etr_max, alpha, ik...
#' @export
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
#' internal function (not for the user) for calculating the regression according to eilers and peeters
#' @param data : data from @seealso generate_regression_eilers_peeters_ETR_I and @seealso generate_regression_eilers_peeters_ETR_II
#' @return internal handover
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

      pm <- NA_real_
      tryCatch(
        {
          pm <- 1 / (b + 2 * sqrt(a * c))
        },
        warning = function(w) {
          message("failed to calculate etr_max: Warning:", w)
        },
        error = function(e) {
          message("failed to calculate etr_max: Error:", e)
        }
      )

      s <- NA_real_
      tryCatch(
        {
          s <- 1 / c
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
      etr_regression_data <- create_regression_data(pars, predictions)

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
        a = a,
        b = b,
        c = c,
        pm = pm,
        s = s,
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

plot_control_eilers_peeters <- function(data, model_result, title, etr_type) {
  library(ggplot2)
  library(glue)
  library(ggthemes)
  library(gridExtra)

  validate_data(data)
  validate_etr_type(etr_type)

  etr_regression_data <- eval(model_result[["etr_regression_data"]])
  validate_regression_data(etr_regression_data)

  data <- remove_det_row_by_etr(data, etr_type)

  values <- c(
    as.character(round(model_result[["sdiff"]], 3)),
    as.character(round(model_result[["a"]], 7)),
    as.character(round(model_result[["b"]], 6)),
    as.character(round(model_result[["c"]], 6)),
    as.character(round(model_result[["pm"]], 3)),
    as.character(round(model_result[["s"]], 3)),
    as.character(round(model_result[["ik"]], 3)),
    as.character(round(model_result[["im"]], 3)),
    as.character(round(model_result[["w"]], 3))
  )

  params <- data.frame(
    Parameter = c("sdiff", "a", "b", "c", "pm", "s", "ik", "im", "w"),
    Value = unlist(values)
  )

  params_transposed <- t(params)
  colnames(params_transposed) <- NULL
  rownames(params_transposed) <- NULL

  yield <- NA_real_
  if (etr_type == etr_I_type) {
    yield <- "Y.I."
  } else {
    yield <- "Y.II."
  }

  etr_regression_data <- model_result[["etr_regression_data"]]
  max_etr <- max(etr_regression_data$prediction)

  plot <- ggplot(data, aes(x = PAR, y = get(etr_type))) +
    geom_point() +
    geom_line(data = etr_regression_data, aes(x = PAR, y = prediction), color = "#f700ff") +
    geom_point(data = data, aes(y = get(yield) * max_etr)) +
    geom_line(data = data, aes(y = get(yield) * max_etr)) +
    labs(x = par_label, y = etr_label, title = eval(title)) +
    scale_y_continuous(
      sec.axis = sec_axis(~ . / max_etr, name = "Yield")
    ) +
    theme_base()

  table <- tableGrob(params_transposed, rows = NULL, theme = ttheme_minimal())
  full_plot <- grid.arrange(plot, table, ncol = 1, heights = c(3, 0.2), widths = 1.5)
  return(full_plot)
}
