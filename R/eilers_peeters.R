eilers_peeters_default_start_value_a <- 0.00004
eilers_peeters_default_start_value_b <- 0.004
eilers_peeters_default_start_value_c <- 5

#' Eilers-Peeters Regression for  ETR I
#'
#' Fits a regression model for ETR I based on Eilers-Peeters (1988), considering photoinhibition.
#'
#' @param data A \code{data.table} from \code{read_dual_pam_data}.
#' @param a_start_value Numeric. Starting value for \eqn{a}. Default: \code{a_start_values_eilers_peeters_default}.
#' @param b_start_value Numeric. Starting value for \eqn{b}. Default: \code{b_start_values_eilers_peeters_default}.
#' @param c_start_value Numeric. Starting value for \eqn{c}. Default: \code{c_start_values_eilers_peeters_default}.
#' 
#'@details
#' A detailed documentation can be found in the README.
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{etr_regression_data}: Predicted ETR values.
#'   \item \code{sdiff}: Deviation between actual and predicted values.
#'   \item \code{a}, \code{b}, \code{c}: Fitted parameters.
#'   \item \code{pm}: Maximum ETR (\eqn{p_m}).
#'   \item \code{s}: Initial slope (\eqn{s}).
#'   \item \code{ik}: Transition point from light limitation to light saturation (\eqn{I_k}).
#'   \item \code{im}: PAR at maximum ETR (\eqn{I_m}).
#'   \item \code{w}: Peak sharpness (\eqn{w}).
#' }
#'
#' @references{
#'   Eilers, P. H. C., & Peeters, J. C. H. (1988). \emph{A model for the relationship between light intensity and the rate of photosynthesis in phytoplankton.} 
#'   Ecological Modelling, 42(3-4), 199-215. Available at: \url{https://doi.org/10.1016/0304-3800(88)90057-9}
#' }
#' @export 
eilers_peeters_generate_regression_ETR_I <- function(
    data,
    a_start_value = eilers_peeters_default_start_value_a,
    b_start_value = eilers_peeters_default_start_value_b,
    c_start_value = eilers_peeters_default_start_value_c) {
  return(eilers_peeters_generate_regression_internal(
    data,
    etr_I_type,
    a_start_value,
    b_start_value,
    c_start_value
  ))
}

#' Eilers-Peeters Regression for  ETR II
#'
#' Fits a regression model for ETR II based on Eilers-Peeters (1988), considering photoinhibition.
#'
#' @param data A \code{data.table} from \code{read_dual_pam_data}.
#' @param a_start_value Numeric. Starting value for \eqn{a}. Default: \code{a_start_values_eilers_peeters_default}.
#' @param b_start_value Numeric. Starting value for \eqn{b}. Default: \code{b_start_values_eilers_peeters_default}.
#' @param c_start_value Numeric. Starting value for \eqn{c}. Default: \code{c_start_values_eilers_peeters_default}.
#' 
#' @details
#' A detailed documentation can be found in the README.
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{etr_regression_data}: Predicted ETR values.
#'   \item \code{sdiff}: Deviation between actual and predicted values.
#'   \item \code{a}, \code{b}, \code{c}: Fitted parameters.
#'   \item \code{pm}: Maximum ETR (\eqn{p_m}).
#'   \item \code{s}: Initial slope (\eqn{s}).
#'   \item \code{ik}: Transition point from light limitation to light saturation (\eqn{I_k}).
#'   \item \code{im}: PAR at maximum ETR (\eqn{I_m}).
#'   \item \code{w}: Peak sharpness (\eqn{w}).
#' }
#'
#' @references{
#'   Eilers, P. H. C., & Peeters, J. C. H. (1988). \emph{A model for the relationship between light intensity and the rate of photosynthesis in phytoplankton.} 
#'   Ecological Modelling, 42(3-4), 199-215. Available at: \url{https://doi.org/10.1016/0304-3800(88)90057-9}
#' }
#' @export 
eilers_peeters_generate_regression_ETR_II <- function(
    data,
    a_start_value = eilers_peeters_default_start_value_a,
    b_start_value = eilers_peeters_default_start_value_b,
    c_start_value = eilers_peeters_default_start_value_c) {
  return(eilers_peeters_generate_regression_internal(
    data,
    etr_II_type,
    a_start_value,
    b_start_value,
    c_start_value
  ))
}

eilers_peeters_generate_regression_internal <- function(
    data,
    etr_type,
    a_start_value = eilers_peeters_default_start_value_a,
    b_start_value = eilers_peeters_default_start_value_b,
    c_start_value = eilers_peeters_default_start_value_c) {
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

      result <- list(
        etr_type = etr_type,
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
      )

      validate_model_result(result)
      return(result)
    },
    warning = function(w) {
      stop("Warning while calculating eilers peeters model: ", w)
    },
    error = function(e) {
      stop("Error while calculating eilers peeters model: ", e)
    }
  )
}

eilers_peeters_modified <- function(model_result) {
  validate_model_result(model_result)
  result <- create_modified_model_result(
    get_etr_type_from_model_result(model_result),
    get_etr_regression_data_from_model_result(model_result),
    get_sdiff_from_model_result(model_result),
    model_result[["a"]],
    model_result[["b"]],
    model_result[["c"]],
    NA_real_,
    model_result[["s"]],
    NA_real_,
    model_result[["pm"]],
    NA_real_,
    model_result[["ik"]],
    NA_real_,
    model_result[["im"]],
    model_result[["w"]],
    NA_real_,
    NA_real_
  )

  return(result)
}
