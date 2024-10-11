platt_default_start_value_alpha <- 0.3
platt_default_start_value_beta <- 0.01
platt_default_start_value_ps <- 30

#' Generate Regression for ETR I using the Platt Model (1980)
#'
#' This function generates a regression model based on the Platt
#' formula. Original naming conventions from the publication are used.
#'
#' @param data A `data.table` containing the input data from \link[pam]{read_pam_data}
#' @param etr_type A character string specifying the column name of the
#' response variable (in this case: ETR I) to be used in the model.
#' @param alpha_start_value_platt Numeric. The starting value for the parameter \eqn{alpha}
#' in the model. Defaults to \code{alpha_start_value_platt_default}.
#' @param beta_start_value_platt Numeric. The starting value for the parameter \eqn{beta}
#' in the model. Defaults to \code{beta_start_value_platt_default}.
#' @param ps_start_value_platt Numeric. The starting value for the parameter \eqn{ps}
#' in the model. Defaults to \code{ps_start_value_platt_default}.
#'
#' @return A list containing the following elements:
#' \describe{
#'   \item{etr_regression_data}{A `data.table` with the predicted values of ETR I to each PAR based on the fitted model.}
#'   \item{sdiff}{The deviation between the actual and predicted ETR values.}
#'   \item{ps}{The maximum electron transport rate without photoinhibition.}
#'   \item{alpha}{The initial slope of the light curve.}
#'   \item{beta}{The photoinhibition of the light curve.}
#'   \item{pm}{The maximum electron transport rate with photoinhibition, calculated as \eqn(pm = ps * (alpha / (alpha + beta)) * ((beta / (alpha + beta))^(beta / alpha)))}
#'   \item{ik}{PAR where the transition point from light limitation to light saturation is achieved with photoinhibition, calculated as \eqn(ik = pm / alpha)}
#'   \item{is}{PAR where the transition point from light limitation to light saturation is achieved without photoinhibition, calculated as \eqn(is = ps / alpha)}
#'   \item{im}{The PAR at which the maximum electron transport rate is achieved with photoinhibition, calculated as \eqn(im = (ps / alpha) * log((alpha + beta) / beta)).}
#'   \item{ib}{ib, calculated as \eqn(ib = ps / beta).}
#' }
#'
#' @details
#' This function uses non-linear least squares fitting to estimate the parameters
#' for the Platt model, which describes the relationship between PAR and
#' ETR I. The model used is:
#'
#' \eqn{p = ps * (1 - e^(-alpha * I) / ps) * (e^(-beta * I) / ps)}
#'
#' It is valid: I = PAR; p = ETR
#'
#' @references
#' Platt, T., Gallegos, C. L., & Harrison, W. G. (1980). Photoinhibition of photosynthesis in natural assemblages of marine phytoplankton.
#' Journal of Marine Research, 38(4). Retrieved from \url{https://elischolar.library.yale.edu/journal_of_marine_research/1525}.

#'
#' @seealso \code{\link{nlsLM}}, \code{\link{minpack.lm}}
#' @importFrom minpack.lm nlsLM
#' @importFrom data.table data.table
#' @export
platt_generate_regression_ETR_I <- function(
    data,
    alpha_start_value = platt_default_start_value_alpha,
    beta_start_value = platt_default_start_value_beta,
    ps_start_value = platt_default_start_value_ps) {
  return(platt_generate_regression_internal(
    data,
    etr_I_type,
    alpha_start_value,
    beta_start_value,
    ps_start_value
  ))
}

#' Generate Regression for ETR II using the Platt Model (1980)
#'
#' This function generates a regression model based on the Platt
#' formula. Original naming conventions from the publication are used.
#'
#' @param data A `data.table` containing the input data from \link[pam]{read_pam_data}
#' @param etr_type A character string specifying the column name of the
#' response variable (in this case: ETR II) to be used in the model.
#' @param alpha_start_value_platt Numeric. The starting value for the parameter \eqn{alpha}
#' in the model. Defaults to \code{alpha_start_value_platt_default}.
#' @param beta_start_value_platt Numeric. The starting value for the parameter \eqn{beta}
#' in the model. Defaults to \code{beta_start_value_platt_default}.
#' @param ps_start_value_platt Numeric. The starting value for the parameter \eqn{ps}
#' in the model. Defaults to \code{ps_start_value_platt_default}.
#'
#' @return A list containing the following elements:
#' \describe{
#'   \item{etr_regression_data}{A `data.table` with the predicted values of ETR II to each PAR based on the fitted model.}
#'   \item{sdiff}{The deviation between the actual and predicted ETR values.}
#'   \item{ps}{The maximum electron transport rate without photoinhibition.}
#'   \item{alpha}{The initial slope of the light curve.}
#'   \item{beta}{The photoinhibition of the light curve.}
#'   \item{pm}{The maximum electron transport rate with photoinhibition, calculated as \eqn{(pm = ps * (alpha / (alpha + beta)) * ((beta / (alpha + beta))^(beta / alpha)))}.}
#'   \item{ik}{PAR where the transition point from light limitation to light saturation is achieved with photoinhibition, calculated as \eqn{(ik = pm / alpha)}.}
#'   \item{is}{PAR where the transition point from light limitation to light saturation is achieved without photoinhibition, calculated as \eqn(is = ps / alpha)}
#'   \item{im}{The PAR at which the maximum electron transport rate is achieved with photoinhibition, calculated as \eqn(im = (ps / alpha) * log((alpha + beta) / beta)).}
#'   \item{ib}{ib, calculated as \eqn(ib = ps / beta).}
#' }
#'
#' @details
#' This function uses non-linear least squares fitting to estimate the parameters
#' for the Platt model, which describes the relationship between PAR and
#' ETR. The model used is:
#'
#' \eqn{p = ps * (1 - e^(-alpha * I) / ps) * (e^(-beta * I) / ps)}
#'
#' It is valid: I = PAR; p = ETR
#'
#' @references
#' Platt, T., Gallegos, C. L., & Harrison, W. G. (1980). Photoinhibition of photosynthesis in natural assemblages of marine phytoplankton.
#' Journal of Marine Research, 38(4). Retrieved from \url{https://elischolar.library.yale.edu/journal_of_marine_research/1525}.

#'
#' @seealso \code{\link{nlsLM}}, \code{\link{minpack.lm}}
#' @importFrom minpack.lm nlsLM
#' @importFrom data.table data.table
#' @export
platt_generate_regression_ETR_II <- function(
    data,
    alpha_start_value = platt_default_start_value_alpha,
    beta_start_value = platt_default_start_value_beta,
    ps_start_value = platt_default_start_value_ps) {
  return(platt_generate_regression_internal(
    data,
    etr_II_type,
    alpha_start_value,
    beta_start_value,
    ps_start_value
  ))
}

platt_generate_regression_internal <- function(
    data,
    etr_type,
    alpha_start_value = platt_default_start_value_alpha,
    beta_start_value = platt_default_start_value_beta,
    ps_start_value = platt_default_start_value_ps) {
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

      result <- list(
        etr_type = etr_type,
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
      )
      validate_model_result(result)
      return(result)
    },
    warning = function(w) {
      stop("Warning while calculating platt model: ", w)
    },
    error = function(e) {
      stop("Error while calculating platt model: ", e)
    }
  )
}

platt_modified <- function(model_result) {
  validate_model_result(model_result)
  result <- create_modified_model_result(
    etr_type = get_etr_type_from_model_result(model_result),
    etr_regression_data = get_etr_regression_data_from_model_result(model_result),
    sdiff = get_sdiff_from_model_result(model_result),
    a = model_result[["ps"]],
    b = model_result[["alpha"]],
    c = model_result[["beta"]],
    d = NA_real_,
    alpha = model_result[["alpha"]],
    beta = model_result[["beta"]],
    etrmax_with_photoinhibition = model_result[["pm"]],
    etrmax_without_photoinhibition = model_result[["ps"]],
    ik_with_photoinhibition = model_result[["ik"]],
    ik_without_photoinhibition = model_result[["is"]],
    im_with_photoinhibition = model_result[["im"]],
    w = NA_real_,
    ib = model_result[["ib"]],
    etrmax_with_without_ratio = model_result[["ps"]] / model_result[["pm"]]
  )

  return(result)
}
