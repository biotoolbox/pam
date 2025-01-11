#' Default start value
#' @export
platt_default_start_value_alpha <- 0.3

#' Default start value
#' @export
platt_default_start_value_beta <- 0.01

#' Default start value
#' @export
platt_default_start_value_ps <- 30

#' Platt Regression for ETR I
#'
#' Fits the Platt (1980) regression model using original naming conventions.
#'
#' @param data A \code{data.table} from \code{read_dual_pam_data}.
#' @param alpha_start_value Numeric. Initial value for \eqn{\alpha}. Default: \code{alpha_start_value_platt_default}.
#' @param beta_start_value Numeric. Initial value for \eqn{\beta}. Default: \code{beta_start_value_platt_default}.
#' @param ps_start_value Numeric. Initial value for \eqn{P_s}. Default: \code{ps_start_value_platt_default}.
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
#' A detailed documentation can be found under \url{https://github.com/biotoolbox/pam?tab=readme-ov-file#platt_generate_regression_etr_i-and-platt_generate_regression_etr_ii} .
#'
#' @references{
#'   Platt, T., Gallegos, C. L., & Harrison, W. G. (1980). \emph{Photoinhibition of photosynthesis in natural assemblages of marine phytoplankton.}
#'   \emph{Journal of Marine Research, 38}(4). Retrieved from \url{https://elischolar.library.yale.edu/journal_of_marine_research/1525/}.
#'
#' }
#' @examples
#' path <- file.path(system.file("extdata", package = "pam"), "20240925.csv")
#' data <- read_dual_pam_data(path)
#'
#' result <- platt_generate_regression_ETR_I(data)
#'
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
#' @param alpha_start_value Numeric. Initial value for \eqn{\alpha}. Default: \code{alpha_start_value_platt_default}.
#' @param beta_start_value Numeric. Initial value for \eqn{\beta}. Default: \code{beta_start_value_platt_default}.
#' @param ps_start_value Numeric. Initial value for \eqn{P_s}. Default: \code{ps_start_value_platt_default}.
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
#' A detailed documentation can be found under \url{https://github.com/biotoolbox/pam?tab=readme-ov-file#platt_generate_regression_etr_i-and-platt_generate_regression_etr_ii}.
#'
#' @references{
#'   Platt, T., Gallegos, C. L., & Harrison, W. G. (1980). \emph{Photoinhibition of photosynthesis in natural assemblages of marine phytoplankton.}
#'   \emph{Journal of Marine Research, 38}(4). Retrieved from \url{https://elischolar.library.yale.edu/journal_of_marine_research/1525/}.
#'
#' }
#' @examples
#' path <- file.path(system.file("extdata", package = "pam"), "20240925.csv")
#' data <- read_dual_pam_data(path)
#'
#' result <- platt_generate_regression_ETR_II(data)
#'
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

      model <- minpack.lm::nlsLM(data[[etr_type]] ~ ps * (1 - exp(-((alpha * PAR) / ps))) * exp(-((beta * PAR) / ps)),
        data = data,
        start = list(
          alpha = alpha_start_value,
          beta = beta_start_value,
          ps = ps_start_value
        ),
        control = stats::nls.control(maxiter = 1000)
      )

      abc <- stats::coef(model)
      alpha <- abc[["alpha"]]
      beta <- abc[["beta"]]
      ps <- abc[["ps"]]

      pm <- NA_real_
      tryCatch(
        {
          pm <- ps * (alpha / (alpha + beta)) * ((beta / (alpha + beta))^(beta / alpha))
        },
        warning = function(w) {
          warning("failed to calculate pm: warning: ", w)
        },
        error = function(e) {
          warning("failed to calculate pm: error: ", e)
        }
      )

      ik <- NA_real_
      tryCatch(
        {
          ik <- pm / alpha
        },
        warning = function(w) {
          warning("failed to calculate ik: warning: ", w)
        },
        error = function(e) {
          warning("failed to calculate ik: error: ", e)
        }
      )

      is <- NA_real_
      tryCatch(
        {
          is <- ps / alpha
        },
        warning = function(w) {
          warning("failed to calculate is: warning: ", w)
        },
        error = function(e) {
          warning("failed to calculate is: error: ", e)
        }
      )

      ib <- NA_real_
      tryCatch(
        {
          ib <- ps / beta
        },
        warning = function(w) {
          warning("failed to calculate ib: warning: ", w)
        },
        error = function(e) {
          warning("failed to calculate ib: error: ", e)
        }
      )

      im <- NA_real_
      tryCatch(
        {
          im <- (ps / alpha) * log((alpha + beta) / beta)
        },
        warning = function(w) {
          warning("failed to calculate im: warning: ", w)
        },
        error = function(e) {
          warning("failed to calculate im: error: ", e)
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
          warning("failed to calculate sdiff: warning: ", w)
        },
        error = function(e) {
          warning("failed to calculate sdiff: error: ", e)
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
      warning("warning while calculating platt model: ", w)
    },
    error = function(e) {
      stop("error while calculating platt model: ", e)
    }
  )
}

#' Platt Model Modification
#'
#' This function enhances the Platt (1980) model by adding parameters not originally included in the model, which were introduced by other models. It also renames parameters to a standardized naming convention used across all models.
#'
#' @param model_result A list containing the results of the model, including parameters such as \code{etr_max}, \code{alpha}, and \code{beta}.
#'
#' @return A modified model result as a list with the following elements:
#' \itemize{
#'   \item \code{etr_type}: ETR Type based on the model result.
#'   \item \code{etr_regression_data}: Regression data with ETR predictions based on the fitted model.
#'   \item \code{sdiff}: The difference between observed and predicted ETR values.
#'   \item \code{a}: Obtained parameter \code{a}, equal to \code{etrmax_without_photoinhibition}.
#'   \item \code{b}: Obtained parameter \code{b}, equal to \code{alpha}.
#'   \item \code{c}: Obtained parameter \code{c}, equal to \code{beta}.
#'   \item \code{d}: Not available, set to \code{NA_real_}.
#'   \item \code{alpha}: The initial slope of the light curve, transferred unchanged as \code{alpha}.
#'   \item \code{beta}: The photoinhibition of the light curve, transferred unchanged as \code{beta}.
#'   \item \code{etrmax_with_photoinhibition}: The maximum electron transport rate with photoinhibition, transferred as \code{pm}.
#'   \item \code{etrmax_without_photoinhibition}: The maximum electron transport rate without photoinhibition, transferred as \code{ps}.
#'   \item \code{ik_with_photoinhibition}: PAR where the transition point from light limitation to light saturation is achieved with photoinhibition, transferred as \code{ik}.
#'   \item \code{ik_without_photoinhibition}: PAR where the transition point from light limitation to light saturation is achieved without photoinhibition, transferred as \code{is}.
#'   \item \code{im_with_photoinhibition}: The PAR at which the maximum electron transport rate is achieved with photoinhibition, transferred as \code{im}.
#'   \item \code{w}: Not available, set to \code{NA_real_}.
#'   \item \code{ib}: Transferred unchanged as \code{ib}.
#'   \item \code{etrmax_with_without_ratio}: Ratio of \code{etrmax_with_photoinhibition} to \code{etrmax_without_photoinhibition}, and \code{ik_with_photoinhibition} to \code{ik_without_photoinhibition}.
#' }
#'
#' @details
#' A detailed documentation can be found under \url{https://github.com/biotoolbox/pam?tab=readme-ov-file#platt_modified}
#'
#' @examples
#' path <- file.path(system.file("extdata", package = "pam"), "20240925.csv")
#' data <- read_dual_pam_data(path)
#'
#' result <- platt_generate_regression_ETR_II(data)
#' modified_result <- platt_modified(result)
#'
#' @export
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
