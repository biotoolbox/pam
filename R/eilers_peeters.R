eilers_peeters_default_start_value_a <- 0.00004
eilers_peeters_default_start_value_b <- 0.004
eilers_peeters_default_start_value_c <- 5

#' Generate Regression for ETR I using the Eilers-Peeters Model (1988)
#'
#' This function generates a regression model based on the Eilers-Peeters
#' formula. Original naming conventions from the publication are used.
#' All parameters are calculated taking photoinhibition into account.
#'
#' @param data A `data.table` containing the input data from \link[pam]{read_pam_data}
#' @param etr_type A character string specifying the column name of the
#' response variable (in this case: ETR I) to be used in the model.
#' @param a_start_value Numeric. The starting value for the parameter \eqn{a}
#' in the model. Defaults to \code{a_start_values_eilers_peeters_default}.
#' @param b_start_value Numeric. The starting value for the parameter \eqn{b}
#' in the model. Defaults to \code{b_start_values_eilers_peeters_default}.
#' @param c_start_value Numeric. The starting value for the parameter \eqn{c}
#' in the model. Defaults to \code{c_start_values_eilers_peeters_default}.
#'
#' @return A list containing the following elements:
#' \describe{
#'   \item{etr_regression_data}{A `data.table` with the predicted values of ETR I to each PAR based on the fitted model.}
#'   \item{sdiff}{The deviation between the actual and predicted ETR values.}
#'   \item{a}{The obtained parameter \eqn{a}.}
#'   \item{b}{The obtained parameter \eqn{b}.}
#'   \item{c}{The obtained parameter \eqn{c}.}
#'   \item{pm}{The maximum electron transport rate, calculated as \eqn{pm = 1 / (b + 2 * \sqrt(a * c))}.}
#'   \item{s}{The initial slope of the light curve, calculated as \eqn{s = 1 / c}.}
#'   \item{ik}{PAR where the transition point from light limitation to light saturation is achieved, calculated as \eqn{ik = c / (b + 2 \sqrt(a * c))}.}
#'   \item{im}{The PAR at which the maximum electron transport rate is achieved, calculated as \eqn{ im = \sqrt(c / a)}.}
#'   \item{w}{The sharpness of the peak, calculated as \eqn{w = b / \sqrt(a * c)}.}
#' }
#'
#' @details
#' This function uses non-linear least squares fitting to estimate the parameters
#' for the Eilers-Peeters model, which describes the relationship between PAR and
#' ETR. The model used is:
#'
#' \eqn{p = I / (a * I^2 + b * I + c)}
#'
#' It is valid: I = PAR; p = ETR
#'
#' @references
#' Eilers, P. H. C., & Peeters, J. C. H. (1988). A model for the relationship
#' between light intensity and the rate of photosynthesis in phytoplankton.
#' Ecological Modelling, 42(3-4), 199-215. \doi{10.1016/0304-3800(88)90057-9}.

#'
#' @seealso \code{\link{nlsLM}}, \code{\link{minpack.lm}}
#' @importFrom minpack.lm nlsLM
#' @importFrom data.table data.table
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

#' Generate Regression for ETR II using the Eilers-Peeters Model (1988)
#'
#' This function generates a regression model based on the Eilers-Peeters
#' formula. Original naming conventions from the publication are used.
#' All parameters are calculated taking photoinhibition into account.
#'
#' @param data A `data.table` containing the input data from \link[pam]{read_pam_data}
#' @param etr_type A character string specifying the column name of the
#' response variable (in this case: ETR II) to be used in the model.
#' @param a_start_value Numeric. The starting value for the parameter \eqn{a}
#' in the model. Defaults to \code{a_start_values_eilers_peeters_default}.
#' @param b_start_value Numeric. The starting value for the parameter \eqn{b}
#' in the model. Defaults to \code{b_start_values_eilers_peeters_default}.
#' @param c_start_value Numeric. The starting value for the parameter \eqn{c}
#' in the model. Defaults to \code{c_start_values_eilers_peeters_default}.
#'
#' @return A list containing the following elements:
#' \describe{
#'   \item{etr_regression_data}{A `data.table` with the predicted values of ETR II to each PAR based on the fitted model.}
#'   \item{sdiff}{The deviation between the actual and predicted ETR values.}
#'   \item{a}{The obtained parameter \eqn{a}.}
#'   \item{b}{The obtained parameter \eqn{b}.}
#'   \item{c}{The obtained parameter \eqn{c}.}
#'   \item{pm}{The maximum electron transport rate, calculated as \eqn{pm = 1 / (b + 2 * \sqrt(a * c))}.}
#'   \item{s}{The initial slope of the light curve, calculated as \eqn{s = 1 / c}.}
#'   \item{ik}{PAR where the transition point from light limitation to light saturation is achieved, calculated as \eqn{ik = c / (b + 2 \sqrt(a * c))}.}
#'   \item{im}{The PAR at which the maximum electron transport rate is achieved, calculated as \eqn{ im = \sqrt(c / a)}.}
#'   \item{w}{The sharpness of the peak, calculated as \eqn{w = b / \sqrt(a * c)}.}
#' }
#'
#' @details
#' This function uses non-linear least squares fitting to estimate the parameters
#' for the Eilers-Peeters model, which describes the relationship between PAR and
#' ETR. The model used is:
#'
#' \eqn{p = I / (a * I^2 + b * I + c)}
#'
#' It is valid: I = PAR; p = ETR
#'
#' @references
#' Eilers, P. H. C., & Peeters, J. C. H. (1988). A model for the relationship
#' between light intensity and the rate of photosynthesis in phytoplankton.
#' Ecological Modelling, 42(3-4), 199-215. \doi{10.1016/0304-3800(88)90057-9}.
#'
#' @seealso \code{\link{nlsLM}}, \code{\link{minpack.lm}}
#' @importFrom minpack.lm nlsLM
#' @importFrom data.table data.table
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
