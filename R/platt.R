platt_default_start_value_alpha <- 0.3
platt_default_start_value_beta <- 0.01
platt_default_start_value_ps <- 30

#' Platt Regression for ETR I
#'
#' Fits the Platt (1980) regression model using original naming conventions.
#'
#' @param data A \code{data.table} from \code{read_dual_pam_data}.
#' @param alpha_start_value_platt Numeric. Initial value for \eqn{\alpha}. Default: \code{alpha_start_value_platt_default}.
#' @param beta_start_value_platt Numeric. Initial value for \eqn{\beta}. Default: \code{beta_start_value_platt_default}.
#' @param ps_start_value_platt Numeric. Initial value for \eqn{P_s}. Default: \code{ps_start_value_platt_default}.
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{etr_regression_data}: Predicted ETR values.
#'   \item \code{sdiff}: Deviation between actual and predicted ETR.
#'   \item \code{ps}: Maximum electron transport rate without photoinhibition (\eqn{P_s}).
#'   \item \code{alpha}: Initial slope of the light curve (\eqn{\alpha}).
#'   \item \code{beta}: Photoinhibition (\eqn{\beta}).
#'   \item \code{pm}: Maximum electron transport rate with photoinhibition (\eqn{P_m}). 
#'   \item \code{ik}: Transition PAR with photoinhibition (\eqn{I_k}).
#'   \item \code{is}: Transition PAR without photoinhibition (\eqn{I_s}).
#'   \item \code{im}: PAR at maximum ETR with photoinhibition (\eqn{I_m}).
#'   \item \code{ib}: (\eqn{I_b})
#' }
#'
#' @details
#' A detailed documentation can be found in the README.
#'
#' @references{
#'   Platt, T., Gallegos, C. L., & Harrison, W. G. (1980). \emph{Photoinhibition of photosynthesis in natural assemblages of marine phytoplankton.} 
#'   \emph{Journal of Marine Research, 38}(4). Retrieved from \url{https://elischolar.library.yale.edu/journal_of_marine_research/1525}.
#'
#' }
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

#' Platt Regression for ETR II
#'
#' Fits the Platt (1980) regression model using original naming conventions.
#'
#' @param data A \code{data.table} from \code{read_dual_pam_data}.
#' @param alpha_start_value_platt Numeric. Initial value for \eqn{\alpha}. Default: \code{alpha_start_value_platt_default}.
#' @param beta_start_value_platt Numeric. Initial value for \eqn{\beta}. Default: \code{beta_start_value_platt_default}.
#' @param ps_start_value_platt Numeric. Initial value for \eqn{P_s}. Default: \code{ps_start_value_platt_default}.
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{etr_regression_data}: Predicted ETR values.
#'   \item \code{sdiff}: Deviation between actual and predicted ETR.
#'   \item \code{ps}: Maximum electron transport rate without photoinhibition (\eqn{P_s}).
#'   \item \code{alpha}: Initial slope of the light curve (\eqn{\alpha}).
#'   \item \code{beta}: Photoinhibition (\eqn{\beta}).
#'   \item \code{pm}: Maximum electron transport rate with photoinhibition (\eqn{P_m}). 
#'   \item \code{ik}: Transition PAR with photoinhibition (\eqn{I_k}).
#'   \item \code{is}: Transition PAR without photoinhibition (\eqn{I_s}).
#'   \item \code{im}: PAR at maximum ETR with photoinhibition (\eqn{I_m}).
#'   \item \code{ib}: (\eqn{I_b})
#' }
#'
#' @details
#' A detailed documentation can be found in the README.
#'
#' @references{
#'   Platt, T., Gallegos, C. L., & Harrison, W. G. (1980). \emph{Photoinhibition of photosynthesis in natural assemblages of marine phytoplankton.} 
#'   \emph{Journal of Marine Research, 38}(4). Retrieved from \url{https://elischolar.library.yale.edu/journal_of_marine_research/1525}.
#'
#' }
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
