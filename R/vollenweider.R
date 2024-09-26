pmax_start_value_vollenweider_default <- 40
a_start_value_vollenweider_default <- 0.1
alpha_start_value_vollenweider_default <- -0.0001
n_start_value_vollenweider_default <- 350

generate_regression_vollenweider_ETR_I <- function(
    data,
    pmax_start_value = a_start_value_vollenweider_default,
    a_start_value = a_start_value_vollenweider_default,
    alpha_start_value = alpha_start_value_vollenweider_default,
    n_start_value = n_start_value_vollenweider_default) {
  return(generate_regression_vollenweider_internal(
    data,
    etr_I_type,
    pmax_start_value,
    a_start_value,
    alpha_start_value,
    n_start_value
  ))
}

generate_regression_vollenweider_ETR_II <- function(
    data,
    pmax_start_value = pmax_start_value_vollenweider_default,
    a_start_value = a_start_value_vollenweider_default,
    alpha_start_value = alpha_start_value_vollenweider_default,
    n_start_value = n_start_value_vollenweider_default) {
  return(generate_regression_vollenweider_internal(
    data,
    etr_II_type,
    pmax_start_value,
    a_start_value,
    alpha_start_value,
    n_start_value
  ))
}

generate_regression_vollenweider_internal <- function(
    data,
    etr_type,
    pmax_start_value = pmax_start_value_vollenweider_default,
    a_start_value = a_start_value_vollenweider_default,
    alpha_start_value = alpha_start_value_vollenweider_default,
    n_start_value = n_start_value_vollenweider_default) {
  library(data.table)
  library(minpack.lm)

  tryCatch(
    {
      validate_etr_type(etr_type)
      validate_data(data)

      if (!is.numeric(pmax_start_value)) {
        stop("pmax start value is not a valid number")
      }
      if (!is.numeric(a_start_value)) {
        stop("a start value is not a valid number")
      }
      if (!is.numeric(alpha_start_value)) {
        stop("alpha start value is not a valid number")
      }
      if (!is.numeric(n_start_value)) {
        stop("n start value is not a valid number")
      }

      data <- remove_det_row_by_etr(data, etr_type)

      model <- nlsLM(
        data[[etr_type]] ~
          pmax * (((a * PAR) / (sqrt(1 + (a * PAR)^2))) * (1 / (sqrt(1 + (alpha * PAR)^2)^n))),
        data = data,
        start = list(
          pmax = pmax_start_value,
          a = a_start_value,
          alpha = alpha_start_value,
          n = n_start_value
        ),
        control = nls.control(maxiter = 1000)
      )

      abc <- coef(model)
      pmax <- abc[["pmax"]]
      a <- abc[["a"]]
      alpha <- abc[["alpha"]]
      n <- abc[["n"]]
      ik <- 1 / a

      popt <- 0
      pars <- c()
      predictions <- c()
      for (p in min(data$PAR):max(data$PAR)) {
        pars <- c(pars, p)
        prediction <- pmax * (((a * p) / (sqrt(1 + (a * p)^2))) * (1 / (sqrt(1 + (alpha * p)^2)^n)))
        predictions <- c(
          predictions,
          prediction
        )

        if (prediction > popt) {
          popt <- prediction
        }
      }

      ik_v <- (ik * popt) / pmax
      alpha_real <- popt / ik_v
      pmax_popt_and_Ik_Ik_v_ratio <- ik / ik_v


      etr_regression_data <- data.table(
        "PAR" = pars,
        "prediction" = predictions
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
        pmax = pmax,
        a = a,
        alpha = alpha,
        n = n,
        ik = ik,
        popt = popt,
        ik_v = ik_v,
        alpha_real = alpha_real,
        pmax_popt_and_Ik_Ik_v_ratio = pmax_popt_and_Ik_Ik_v_ratio
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

plot_control_vollenweider <- function(data, regression_data, title, etr_type) {
  library(ggplot2)
  library(glue)
  validate_data(data)

  etr_regression_data <- eval(regression_data[["etr_regression_data"]])

  # Create plot for ETR.II. by PAR and filename
  plot <- ggplot(data, aes(x = PAR, y = get(etr_type))) +
    geom_point() +
    geom_line(data = etr_regression_data, aes(x = PAR, y = prediction), color = "#e70f1f") +
    labs(x = par_label, y = etr_label, title = eval(title)) +
    theme_minimal() +
  labs(caption = glue("pmax: {round(regression_data[['pmax']], 3)}
  popt: {round(regression_data[['popt']], 3)}
  alpha: {round(regression_data[['alpha']], 3)}
  alpha_real: {round(regression_data[['alpha_real']], 3)}
  ik: {round(regression_data[['ik']], 3)}
  ik_v: {round(regression_data[['ik_v']], 3)}
  pmax_popt_and_Ik_Ik_v_ratio: {round(regression_data[['pmax_popt_and_Ik_Ik_v_ratio']], 3)}
  a: {round(regression_data[['a']], 3)}
  n: {round(regression_data[['n']], 3)}
  SDiff: {round(regression_data[['sdiff']], 3)}")) +
    theme(plot.caption = element_text(hjust = 0))

  return(plot)
}
