alpha_start_value_platt_default <- 0.3
beta_start_value_platt_default <- 0.01
ps_start_value_platt_default <- 30

generate_regression_platt_ETR_I <- function(
    data,
    alpha_start_value = alpha_start_value_platt_default,
    beta_start_value = beta_start_value_platt_default,
    ps_start_value = ps_start_value_platt_default) {
  return(generate_regression_platt_internal(
    data,
    etr_I_type,
    alpha_start_value,
    beta_start_value,
    ps_start_value
  ))
}

generate_regression_platt_ETR_II <- function(
    data,
    alpha_start_value = alpha_start_value_platt_default,
    beta_start_value = beta_start_value_platt_default,
    ps_start_value = ps_start_value_platt_default) {
  return(generate_regression_platt_internal(
    data,
    etr_II_type,
    alpha_start_value,
    beta_start_value,
    ps_start_value
  ))
}

generate_regression_platt_internal <- function(
    data,
    etr_type,
    alpha_start_value = alpha_start_value_platt_default,
    beta_start_value = beta_start_value_platt_default,
    ps_start_value = ps_start_value_platt_default) {
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
      if (!is.numeric(ps_start_value)) {
        stop("ps start value is not a valid number")
      }

      data <- remove_det_row_by_etr(data, etr_type)

      model <- nlsLM(data[[etr_type]] ~ ps * (1 - exp(-((alpha * PAR) / ps))) * exp(-((beta * PAR) / ps)),
        data = data,
        start = list(
          alpha = alpha_start_value,
          beta = beta_start_value,
          ps = ps_start_value
        ),
        control = nls.control(maxiter = 1000)
      )

      abc <- coef(model)
      alpha <- abc[["alpha"]]
      beta <- abc[["beta"]]
      ps <- abc[["ps"]]

      pm <- NA_real_
      tryCatch(
        {
          pm <- ps * (alpha / (alpha + beta)) * ((beta / (alpha + beta))^(beta / alpha))
        },
        warning = function(w) {
          message("failed to calculate pm: Warning:", w)
        },
        error = function(e) {
          message("failed to calculate pm: Error:", e)
        }
      )

      ik <- NA_real_
      tryCatch(
        {
          ik <- pm / alpha
        },
        warning = function(w) {
          message("failed to calculate ik: Warning:", w)
        },
        error = function(e) {
          message("failed to calculate ik: Error:", e)
        }
      )

      is <- NA_real_
      tryCatch(
        {
          is <- ps / alpha
        },
        warning = function(w) {
          message("failed to calculate is: Warning:", w)
        },
        error = function(e) {
          message("failed to calculate is: Error:", e)
        }
      )

      ib <- NA_real_
      tryCatch(
        {
          ib <- ps / beta
        },
        warning = function(w) {
          message("failed to calculate ib: Warning:", w)
        },
        error = function(e) {
          message("failed to calculate ib: Error:", e)
        }
      )

      im <- NA_real_
      tryCatch(
        {
          im <- (ps / alpha) * log((alpha + beta) / beta)
        },
        warning = function(w) {
          message("failed to calculate im: Warning:", w)
        },
        error = function(e) {
          message("failed to calculate im: Error:", e)
        }
      )

      pars <- c()
      predictions <- c()
      for (p in min(data$PAR):max(data$PAR)) {
        pars <- c(pars, p)
        predictions <- c(predictions, ps * (1 - exp((-alpha * p) / ps)) * exp((-beta * p) / ps))
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
        alpha = alpha,
        beta = beta,
        ps = ps,
        pm = pm,
        ik = ik,
        is = is,
        ib = ib,
        im = im
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

plot_control_platt <- function(data, model_result, etr_type, title) {
  values <- c(
    as.character(round(model_result[["sdiff"]], 3)),
    as.character(round(model_result[["alpha"]], 7)),
    as.character(round(model_result[["beta"]], 6)),
    as.character(round(model_result[["ps"]], 6)),
    as.character(round(model_result[["pm"]], 3)),
    as.character(round(model_result[["ik"]], 3)),
    as.character(round(model_result[["is"]], 3)),
    as.character(round(model_result[["ib"]], 3)),
    as.character(round(model_result[["im"]], 3))
  )

  params <- data.frame(
    Parameter = c("sdiff", "alpha", "beta", "ps", "pm", "ik", "is", "ib", "im"),
    Value = unlist(values)
  )

  return(
    plot_control(
      data,
      model_result,
      etr_type,
      title,
      color_platt,
      params
    )
  )
}
